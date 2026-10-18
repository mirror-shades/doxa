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
const Location = @import("../../utils/reporting.zig").Location;
const GroupTable = @import("../../common/group_table.zig").GroupTable;
const names = @import("names.zig");
const helpers = @import("helpers.zig");
const returns = @import("returns.zig");
const cast = @import("infer/cast.zig");
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
    for (case.path_patterns) |*path| {
        if (path.tokens.len == 0) continue;
        resolved[path.pattern] = try resolvePathPattern(self, path, group);
        if (path.field_names.len > 0) try checkDestructure(self, path.*, resolved[path.pattern]);
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
pub fn inferMatchCaseTypeWithNarrow(self: *SemanticAnalyzer, case: ast.MatchCase, subject_type: *const ast.TypeInfo, subject: *const ast.Expr, discarded: bool) ErrorList!*ast.TypeInfo {
    // A per-case scope keeps narrowing from leaking out of the arm.
    const case_scope = try self.memory.scope_manager.createScope(self.current_scope, self.memory);
    defer case_scope.deinit();

    const prev_scope = self.current_scope;
    self.current_scope = case_scope;
    defer self.current_scope = prev_scope;

    // An arm narrows the subject only when it lists a single pattern: with
    // several, any of them could have selected it, so the body sees the
    // subject as declared. A variable or a field path is narrowed in place
    // (`cast.narrowSubject`); codegen reads the narrowing from each read of
    // the subject, and nothing re-derives it.
    const narrows = case.patterns.len == 1;

    if (case.path_patterns.len > 0) {
        const path = case.path_patterns[0];
        if (path.tokens.len >= 1) {
            const member: ?TypeRef = switch (case.resolved[path.pattern]) {
                .type => |ref| ref,
                .token, .variant => null,
            };
            if (narrows) {
                if (member) |ref| try cast.narrowSubject(self, case_scope, subject, try narrowedType(self, .{ .base = .Custom, .custom_type = .{ .ref = ref }, .is_mutable = false }));
            }

            // Struct destructuring binds each field name as a local.
            // These are the arm's own names, not the subject, so they
            // bind whatever the subject is and whether or not the arm narrows.
            if (path.field_names.len > 0) {
                if (member) |ref| if (self.struct_table.idOf(ref)) |sid| {
                    const fields = self.struct_table.fields(sid) orelse &.{};
                    const storages = try self.allocator.alloc(u32, path.field_names.len);
                    // Every name binds, or none is attached: an
                    // undeclared field is `checkDestructure`'s error,
                    // which stops the compile before codegen, and no
                    // storage of the arm is left unset.
                    const bound = for (path.field_names, storages) |field_token, *storage| {
                        const field = for (fields) |f| {
                            if (std.mem.eql(u8, f.name, field_token.lexeme)) break f;
                        } else break false;
                        self.checkFreshName(case_scope, field_token);
                        storage.* = (try bindNarrowed(case_scope, field_token.lexeme, field.type_info.*, self.allocator)).storage_id;
                    } else true;
                    if (bound) case.path_patterns[0].field_storages = storages else self.allocator.free(storages);
                };
            }
        }
    } else if (narrows) {
        if (try patternType(self, case.patterns[0], case.resolved[0], subject_type)) |narrowed| {
            try cast.narrowSubject(self, case_scope, subject, try narrowedType(self, narrowed));
        }
    }

    // An arm's value is the match's: discarded when the match's is.
    self.discarded_grant = discarded;
    const case_body_type = try infer_type.inferTypeFromExpr(self, case.body);
    if (cast.narrowedViewName(subject)) |name| case_scope.propagateUsedToParent(name);
    return case_body_type;
}

/// `narrowed`, owned by the analysis allocator so a narrowing can hold it.
fn narrowedType(self: *SemanticAnalyzer, narrowed: ast.TypeInfo) ErrorList!*ast.TypeInfo {
    const type_info = try ast.TypeInfo.createDefault(self.allocator);
    type_info.* = narrowed;
    return type_info;
}

pub fn bindNarrowed(scope: *Scope, name: []const u8, narrowed: ast.TypeInfo, allocator: std.mem.Allocator) ErrorList!*Variable {
    const type_info = try ast.TypeInfo.createDefault(allocator);
    type_info.* = narrowed;
    return scope.createValueBinding(name, type_info, false);
}

/// What a match path denotes. Against a group subject the path names a
/// member, and possibly a variant inside it, which is recorded on the path
/// (`groupPath`). Otherwise the path spells a type outright — bare or
/// module-qualified (`std.json.Node`) — or an enum variant (`ns.Color.Red`),
/// whose enum is everything ahead of the variant.
pub fn resolvePathPattern(self: *SemanticAnalyzer, path: *ast.MatchCase.PathPattern, group: ?TypeRef) ErrorList!ast.MatchCase.Resolved {
    if (group) |group_ref| {
        if (try groupPath(self, path, group_ref)) |member| return .{ .type = member };
    }
    if (try names.resolveTypeName(self, try joinTokens(self, path.tokens), self.current_module)) |ref| return .{ .type = ref };
    if (path.field_names.len > 0 or path.tokens.len < 2) return .token;
    const prefix = try joinTokens(self, path.tokens[0 .. path.tokens.len - 1]);
    const ref = (try names.resolveTypeName(self, prefix, self.current_module)) orelse return .token;
    const custom = (try self.customType(ref)) orelse return .token;
    return if (custom.kind == .Enum) .{ .variant = ref } else .token;
}

/// The member of `group` a match path names, recording on the path the
/// variant inside it. Written through the group, the path is
/// `Group.Member`, `Group.Member.Variant`, or `Group.Variant` for a variant
/// unique across the members (`docs/groups.md` 2.3). Otherwise it starts at
/// the member, however the member is reached (`IOError`, `io.IOErr`), and
/// may go on to a variant (`io.IOErr.Denied`). A destructuring or wildcard
/// path ends at its member.
fn groupPath(self: *SemanticAnalyzer, path: *ast.MatchCase.PathPattern, group: TypeRef) ErrorList!?TypeRef {
    // A wildcard path ends in its `*`.
    const tokens = if (path.is_wildcard) path.tokens[0 .. path.tokens.len - 1] else path.tokens;
    const whole = path.is_wildcard or path.field_names.len > 0;
    const through_group = tokens.len >= 2 and if (try names.resolveTypeName(self, tokens[0].lexeme, self.current_module)) |ref| ref.eql(group) else false;
    if (through_group) {
        const rest = tokens[1..];
        if (try self.groupMemberType(group, rest[0].lexeme)) |member| {
            if (rest.len > 2 or (whole and rest.len > 1)) return null;
            if (rest.len == 2) path.variant = rest[1];
            return member;
        }
        if (rest.len != 1 or whole) return null;
        const member = (try groupVariantMember(self, group, rest[0])) orelse return null;
        path.variant = rest[0];
        return member;
    }
    if (try memberNamed(self, group, tokens)) |member| return member;
    if (whole or tokens.len < 2) return null;
    const member = (try memberNamed(self, group, tokens[0 .. tokens.len - 1])) orelse return null;
    path.variant = tokens[tokens.len - 1];
    return member;
}

/// The member of `group` that `tokens` name: its qualifier, or a type name,
/// bare or module-qualified, that resolves to the member.
fn memberNamed(self: *SemanticAnalyzer, group: TypeRef, tokens: []const Token) ErrorList!?TypeRef {
    if (tokens.len == 1) {
        if (try self.groupMemberType(group, tokens[0].lexeme)) |member| return member;
    }
    const ref = (try names.resolveTypeName(self, try joinTokens(self, tokens), self.current_module)) orelse return null;
    try self.ensureTypes(group.module);
    const id = self.group_table.idOf(group) orelse return null;
    for (self.group_table.members(id) orelse &.{}) |member| {
        if (member.ref.eql(ref)) return member.ref;
    }
    return null;
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

/// The enum member of `group` declaring the variant `name`, for `Group.Variant`
/// (`docs/groups.md` 2.3). A variant no member declares, or one more than one
/// member declares, is reported here, and gives null.
pub fn groupVariantMember(self: *SemanticAnalyzer, group: TypeRef, name: Token) ErrorList!?TypeRef {
    try self.ensureTypes(group.module);
    const id = self.group_table.idOf(group) orelse return null;
    var declaring = std.array_list.Managed(GroupTable.Member).init(self.allocator);
    defer declaring.deinit();
    for (self.group_table.members(id) orelse &.{}) |member| {
        if (member.kind != .Enum) continue;
        if (try enumDeclares(self, member.ref, name.lexeme)) try declaring.append(member);
    }
    const location = ast.SourceSpan.fromToken(name).location;
    switch (declaring.items.len) {
        0 => self.reporter.reportCompileError(
            location,
            ErrorCode.VARIABLE_NOT_FOUND,
            "Group '{s}' has no member declaring the variant '{s}'",
            .{ group.name, name.lexeme },
        ),
        1 => return declaring.items[0].ref,
        else => {
            var listed = std.array_list.Managed(u8).init(self.allocator);
            defer listed.deinit();
            for (declaring.items, 0..) |member, i| {
                if (i > 0) try listed.appendSlice("', '");
                try listed.appendSlice(member.qualifier);
            }
            self.reporter.reportCompileError(
                location,
                ErrorCode.AMBIGUOUS_GROUP_VARIANT,
                "'{s}.{s}' is ambiguous: members '{s}' all declare the variant '{s}'. Name the member: '{s}.{s}.{s}'",
                .{ group.name, name.lexeme, listed.items, name.lexeme, group.name, declaring.items[0].qualifier, name.lexeme },
            );
        },
    }
    self.fatal_error = true;
    return null;
}

/// Whether enum `ref` declares the variant `name`.
pub fn enumDeclares(self: *SemanticAnalyzer, ref: TypeRef, name: []const u8) ErrorList!bool {
    const custom = (try self.customType(ref)) orelse return false;
    for (custom.enum_variants orelse &.{}) |variant| {
        if (std.mem.eql(u8, variant.name, name)) return true;
    }
    return false;
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

/// Whether the arms of `match_expr` reach every value its subject can hold:
/// it ends in `else`, its subject is an enum or a group (whose uncovered
/// members are reported where the match is analyzed, E1033), or its subject
/// is a union every member of which an arm names.
pub fn matchCovers(self: *SemanticAnalyzer, match_expr: ast.MatchExpr, subject_type: *const ast.TypeInfo) ErrorList!bool {
    if (helpers.matchHasElseArm(match_expr)) return true;
    if (subject_type.base == .Union) return (try uncoveredUnionMember(self, match_expr, subject_type)) == null;
    if (helpers.matchSubjectGroup(self, subject_type) != null) return true;
    const custom = subject_type.custom_type orelse return false;
    const custom_type = (try self.customType(custom.resolved())) orelse return false;
    return custom_type.kind == .Enum;
}

/// A match whose value is used has one on every path: over a union, every
/// member needs an arm or the match an `else`, since an uncovered member
/// would leave it without a value.
pub fn checkUnionMatchExhaustive(self: *SemanticAnalyzer, match_expr: ast.MatchExpr, subject_type: *const ast.TypeInfo, location: Location) ErrorList!void {
    if (subject_type.base != .Union or helpers.matchHasElseArm(match_expr)) return;
    const missing = (try uncoveredUnionMember(self, match_expr, subject_type)) orelse return;
    self.reporter.reportCompileError(
        location,
        ErrorCode.NON_EXHAUSTIVE_MATCH,
        "Match on {s} is not exhaustive: {s} is not covered, and the match's value is used. Cover every member or add an 'else' arm",
        .{ try helpers.typeSpelling(self, subject_type), try helpers.typeSpelling(self, missing) },
    );
    self.fatal_error = true;
}

/// The first member of union `subject_type` that no arm of `match_expr` names.
fn uncoveredUnionMember(self: *SemanticAnalyzer, match_expr: ast.MatchExpr, subject_type: *const ast.TypeInfo) ErrorList!?*const ast.TypeInfo {
    const union_type = subject_type.union_type orelse return null;
    for (union_type.types) |member| {
        if (!try memberCovered(self, match_expr, member, subject_type)) return member;
    }
    return null;
}

/// Whether an arm names `member`, or a group holding it. A group member of
/// the union is covered when every one of the group's members is.
fn memberCovered(self: *SemanticAnalyzer, match_expr: ast.MatchExpr, member: *const ast.TypeInfo, subject_type: *const ast.TypeInfo) ErrorList!bool {
    for (match_expr.cases) |case| {
        // An arm's patterns are resolved before its match is checked.
        if (case.resolved.len != case.patterns.len) continue;
        for (case.patterns, case.resolved) |pattern, resolved| {
            const arm = (try patternType(self, pattern, resolved, subject_type)) orelse continue;
            if (helpers.typesEqual(self, &arm, member) or returns.namedMemberAccepts(self, &arm, member)) return true;
        }
    }
    const group = helpers.matchSubjectGroup(self, member) orelse return false;
    const group_id = self.group_table.idOf(group) orelse return false;
    for (self.group_table.members(group_id) orelse &.{}) |group_member| {
        const as_type: ast.TypeInfo = .{ .base = .Custom, .custom_type = .{ .ref = group_member.ref }, .is_mutable = false };
        if (!try memberCovered(self, match_expr, &as_type, subject_type)) return false;
    }
    return true;
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
