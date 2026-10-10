const std = @import("std");
const ast = @import("../../ast/ast.zig");
const TypeRef = ast.TypeRef;
const Reporting = @import("../../utils/reporting.zig");
const SemanticAnalyzer = @import("semantic.zig").SemanticAnalyzer;
const ErrorCode = @import("../../utils/errors.zig").ErrorCode;
const ErrorList = @import("../../utils/errors.zig").ErrorList;
const StructTable = @import("../../common/struct_table.zig").StructTable;
const GroupTable = @import("../../common/group_table.zig").GroupTable;
const Types = @import("../../types/types.zig");
const CustomTypeInfo = Types.CustomTypeInfo;
const StructField = Types.StructField;
const HIRTypeModule = @import("../../codegen/hir/types.zig");
const HIRType = HIRTypeModule.HIRType;
const StructId = HIRTypeModule.StructId;
const names = @import("names.zig");
const infer_type = @import("infer_type.zig");
const graph_mod = @import("../../module/graph.zig");

/// The resolved identity of a named type, or null for an anonymous one.
fn refOf(type_info: *const ast.TypeInfo) ?TypeRef {
    const custom = type_info.custom_type orelse return null;
    return custom.resolved();
}

/// Whether `base` spells a named type: an enum, a struct, or a group or type
/// written by name.
fn isNamedBase(base: ast.Type) bool {
    return base == .Enum or base == .Struct or base == .Custom;
}

/// Structural equality for TypeInfo. Named types are equal exactly when they
/// are the same declaration: two modules' `Node` are different types. A
/// resolved named type is the same type however it is spelled (`.Enum`,
/// `.Struct` or `.Custom`).
pub fn typesEqual(self: *const SemanticAnalyzer, a: *const ast.TypeInfo, b: *const ast.TypeInfo) bool {
    if (isNamedBase(a.base) and isNamedBase(b.base)) {
        // A `.Blue` shorthand its context has not typed yet carries no type of
        // its own and is compatible with any named type; otherwise identities
        // must agree.
        const ra = refOf(a) orelse return isUntypedVariant(a) or a.base == .Custom;
        const rb = refOf(b) orelse return isUntypedVariant(b) or b.base == .Custom;
        return ra.eql(rb);
    }
    if (a.base != b.base) return false;

    switch (a.base) {
        .Int, .Byte, .Float, .String, .Tetra, .Nothing => return true,

        .Enum, .Struct, .Custom => unreachable,

        .Array => {
            // An array literal without element type info is compatible with any array.
            if (a.array_type != null and b.array_type != null) {
                return typesEqual(self, a.array_type.?, b.array_type.?);
            }
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
            // Flattened before compare; compared as sets (order-independent).
            if (a.union_type == null or b.union_type == null) return false;
            const au = a.union_type.?;
            const bu = b.union_type.?;
            if (au.types.len != bu.types.len) return false;
            for (au.types) |amt| {
                for (bu.types) |bmt| {
                    if (typesEqual(self, amt, bmt)) break;
                } else return false;
            }
            return true;
        },
    }
}

/// The name a diagnostic shows for a type.
pub fn typeLabel(type_info: *const ast.TypeInfo) []const u8 {
    if (type_info.custom_type) |custom| return custom.displayName();
    return @tagName(type_info.base);
}

/// The names a diagnostic shows for two types it compares. Two distinct types
/// that share a declared name (`a.Node`, `b.Node`) are told apart by the
/// stable names of the modules that declare them.
pub fn typeLabels(self: *const SemanticAnalyzer, a: *const ast.TypeInfo, b: *const ast.TypeInfo) !struct { []const u8, []const u8 } {
    const plain = .{ typeLabel(a), typeLabel(b) };
    const ref_a = refOf(a) orelse return plain;
    const ref_b = refOf(b) orelse return plain;
    if (ref_a.eql(ref_b) or !std.mem.eql(u8, ref_a.name, ref_b.name)) return plain;
    return .{ try qualifiedLabel(self, ref_a), try qualifiedLabel(self, ref_b) };
}

fn qualifiedLabel(self: *const SemanticAnalyzer, ref: TypeRef) ![]const u8 {
    return std.fmt.allocPrint(self.allocator, "{s} (from {s})", .{ ref.name, self.graph.moduleName(ref.module) });
}

/// The group a type names, when it names one.
fn groupOf(self: *const SemanticAnalyzer, type_info: *const ast.TypeInfo) ?TypeRef {
    if (type_info.base != .Custom) return null;
    const ref = refOf(type_info) orelse return null;
    const custom = self.custom_types.get(ref) orelse return null;
    return if (custom.kind == .Group) ref else null;
}

/// `.Red` names a variant of the enum its context expects, and nothing else
/// types it. Retype the shorthand `expr`, inferred as `actual`, by the
/// `expected` type of its position. A branching expression yields one of its
/// branches and an array literal holds its elements, so each sits in the same
/// position, and the enclosing expression takes the enum they agree on.
pub fn contextualizeEnumMember(self: *const SemanticAnalyzer, expr: *const ast.Expr, actual: *ast.TypeInfo, expected: *const ast.TypeInfo) void {
    if (expr.data == .Array) {
        const element_expected = expected.array_type orelse return;
        const element_actual = actual.array_type orelse return;
        const ref = contextualizeAll(self, expr.data.Array, element_expected) orelse return;
        retype(element_actual, ref);
        return;
    }
    const ref = contextualize(self, expr, expected) orelse return;
    retype(actual, ref);
}

/// Type every shorthand `expr` yields from `expected`, and return the enum
/// `expr` then yields, or null when it yields none or its branches disagree.
fn contextualize(self: *const SemanticAnalyzer, expr: *const ast.Expr, expected: *const ast.TypeInfo) ?TypeRef {
    const ref = switch (expr.data) {
        .EnumMember => |member| contextualEnum(self, expected, member.lexeme),
        .If => |if_expr| agree(contextualizeBranch(self, if_expr.then_branch, expected), contextualizeBranch(self, if_expr.else_branch, expected)),
        .Match => |match_expr| blk: {
            var ref: ?TypeRef = null;
            for (match_expr.cases, 0..) |case, i| {
                const arm = contextualizeBranch(self, case.body, expected);
                ref = if (i == 0) arm else agree(ref, arm);
            }
            break :blk ref;
        },
        .Block => |block| contextualizeBranch(self, block.value, expected),
        .Grouping => |inner| contextualizeBranch(self, inner, expected),
        else => null,
    } orelse return null;
    if (self.type_cache.get(expr.base.id)) |cached| retype(cached, ref);
    return ref;
}

/// The enum a branch yields once contextualized: what its shorthands take,
/// or the named type it already has.
fn contextualizeBranch(self: *const SemanticAnalyzer, branch: ?*const ast.Expr, expected: *const ast.TypeInfo) ?TypeRef {
    const expr = branch orelse return null;
    if (contextualize(self, expr, expected)) |ref| return ref;
    const inferred = self.type_cache.get(expr.base.id) orelse return null;
    return refOf(inferred);
}

/// The enum every element of an array literal takes in `expected`'s element
/// position, or null when they do not all agree.
fn contextualizeAll(self: *const SemanticAnalyzer, elements: []const *ast.Expr, expected: *const ast.TypeInfo) ?TypeRef {
    var ref: ?TypeRef = null;
    for (elements, 0..) |element, i| {
        const element_ref = contextualizeBranch(self, element, expected);
        ref = if (i == 0) element_ref else agree(ref, element_ref);
    }
    return ref;
}

fn agree(a: ?TypeRef, b: ?TypeRef) ?TypeRef {
    const left = a orelse return null;
    const right = b orelse return null;
    return if (left.eql(right)) left else null;
}

/// Report a shorthand `expr` whose `expected` enum does not declare it, and
/// type it as that enum so it is not also reported as untyped. Returns whether
/// it reported.
pub fn reportUndeclaredVariant(self: *SemanticAnalyzer, expr: *const ast.Expr, actual: *ast.TypeInfo, expected: *const ast.TypeInfo, span: ast.SourceSpan) bool {
    if (expr.data != .EnumMember or !isUntypedVariant(actual)) return false;
    const ref = refOf(expected) orelse return false;
    const custom = self.custom_types.get(ref) orelse return false;
    if (custom.kind != .Enum) return false;
    self.reporter.reportCompileError(
        span.location,
        ErrorCode.TYPE_MISMATCH,
        "'{s}' has no variant '{s}'",
        .{ ref.name, expr.data.EnumMember.lexeme },
    );
    self.fatal_error = true;
    retype(actual, ref);
    return true;
}

/// Give an untyped shorthand's type the enum its context chose. A type that
/// is already named keeps its identity.
fn retype(type_info: *ast.TypeInfo, ref: TypeRef) void {
    if (!isUntypedVariant(type_info)) return;
    type_info.* = .{ .base = .Custom, .custom_type = .{ .ref = ref } };
}

/// The type of a `.Variant` shorthand its context has not typed yet.
pub fn isUntypedVariant(type_info: *const ast.TypeInfo) bool {
    return type_info.base == .Enum and type_info.custom_type == null;
}

/// The enum a `.variant` shorthand denotes where `expected` is wanted: the
/// expected enum itself, or the single enum member of an expected group that
/// declares the variant.
fn contextualEnum(self: *const SemanticAnalyzer, expected: *const ast.TypeInfo, variant: []const u8) ?TypeRef {
    const ref = refOf(expected) orelse return null;
    const custom = self.custom_types.get(ref) orelse return null;
    switch (custom.kind) {
        .Enum => return if (declaresVariant(custom, variant)) ref else null,
        .Group => {
            const group_id = self.group_table.idOf(ref) orelse return null;
            var found: ?TypeRef = null;
            for (self.group_table.members(group_id) orelse &.{}) |member| {
                if (member.kind != .Enum) continue;
                const member_type = self.custom_types.get(member.ref) orelse continue;
                if (!declaresVariant(member_type, variant)) continue;
                if (found != null) return null; // two members declare it: ambiguous
                found = member.ref;
            }
            return found;
        },
        .Struct => return null,
    }
}

fn declaresVariant(custom: CustomTypeInfo, variant: []const u8) bool {
    for (custom.enum_variants orelse &.{}) |declared| {
        if (std.mem.eql(u8, declared.name, variant)) return true;
    }
    return false;
}

fn typeMatchesUnionMember(self: *const SemanticAnalyzer, exp_member: *const ast.TypeInfo, actual_member: *const ast.TypeInfo) bool {
    if (typesEqual(self, exp_member, actual_member)) return true;
    // A group accepts each of its members.
    if (groupOf(self, exp_member)) |group| return typeWidensToGroup(self, group, actual_member);
    return false;
}

fn typeIsGroupMember(self: *const SemanticAnalyzer, group: TypeRef, actual: *const ast.TypeInfo) bool {
    const actual_ref = refOf(actual) orelse return false;
    const group_id = self.group_table.idOf(group) orelse return false;
    for (self.group_table.members(group_id) orelse return false) |member| {
        if (member.ref.eql(actual_ref)) return true;
    }
    return false;
}

/// True when `actual` is assignable to `group` on its own: it is one of the
/// group's members, it is the group itself, or it is `Nothing`.
///
/// The last two cases are why this exists separately from `typeIsGroupMember`.
/// A function declared `returns SomeGroup` commonly has one path returning a
/// member and another falling off the end with a bare `return` (typed
/// `Nothing`), and a wrapper returns the group unchanged. Both are legitimate,
/// and both appear as *members* when such a body's inferred return type is a
/// union — so a union is assignable to the group exactly when each of its
/// members is, and asking only "is this member one of the group's members?"
/// rejects the group itself and `Nothing`.
fn typeWidensToGroup(self: *const SemanticAnalyzer, group: TypeRef, actual: *const ast.TypeInfo) bool {
    if (actual.base == .Nothing) return true;
    if (actual.base == .Custom) {
        if (refOf(actual)) |ref| if (ref.eql(group)) return true;
    }
    return typeIsGroupMember(self, group, actual);
}

/// §4.1: a `match` on a group must cover every flattened member. An arm covers
/// a whole member when it names the member without reaching inside it — a bare
/// member pattern, a destructuring pattern, or a domain wildcard. An arm that
/// names a variant covers only that variant, so an enum member is covered once
/// every one of its variants appears. An `else` arm covers everything.
pub fn checkGroupMatchExhaustive(
    self: *SemanticAnalyzer,
    match_expr: ast.MatchExpr,
    subject_type: *const ast.TypeInfo,
    location: Reporting.Location,
) !void {
    const group = matchSubjectGroup(self, subject_type) orelse return;
    const group_id = self.group_table.idOf(group) orelse return;
    const members = self.group_table.members(group_id) orelse return;
    if (members.len == 0) return;
    if (matchHasElseArm(match_expr)) return;

    const allocator = self.allocator;
    var coverage = try allocator.alloc(MemberCoverage, members.len);
    defer {
        for (coverage) |member_coverage| {
            if (member_coverage.enum_coverage) |enum_coverage| allocator.free(enum_coverage.covered);
        }
        allocator.free(coverage);
    }
    @memset(coverage, .{});

    for (members, 0..) |member, index| {
        if (member.kind != .Enum) continue;
        const enum_type = (try self.customType(member.ref)) orelse continue;
        const variants = enum_type.enum_variants orelse continue;
        const covered = try allocator.alloc(bool, variants.len);
        @memset(covered, false);
        coverage[index].enum_coverage = .{ .variants = variants, .covered = covered };
    }

    for (match_expr.cases) |case| {
        // The path patterns decide an arm whenever the case has any; the bare
        // pattern list repeats their last token.
        if (case.path_patterns.len > 0) {
            for (case.path_patterns) |path_pattern| {
                if (path_pattern.tokens.len == 0) continue;
                const split = path_pattern.split(group.name);
                const index = groupMemberIndexOf(members, split.member.lexeme) orelse continue;
                const reaches_inside = !path_pattern.is_wildcard and
                    path_pattern.field_names.len == 0 and
                    split.variant != null;
                if (!reaches_inside) {
                    coverage[index].whole = true;
                } else {
                    markVariantCovered(&coverage[index], split.variant.?.lexeme);
                }
            }
            continue;
        }

        for (case.patterns) |pattern| {
            if (pattern.type == .ELSE or std.mem.eql(u8, pattern.lexeme, "else")) continue;
            const index = groupMemberIndexOf(members, pattern.lexeme) orelse continue;
            coverage[index].whole = true;
        }
    }

    var uncovered = std.array_list.Managed([]const u8).init(allocator);
    defer uncovered.deinit();
    for (members, 0..) |member, index| {
        if (isMemberCovered(coverage[index])) continue;
        try uncovered.append(member.qualifier);
    }
    if (uncovered.items.len == 0) return;

    const listed = try std.mem.join(allocator, "', '", uncovered.items);
    defer allocator.free(listed);
    self.reporter.reportCompileError(
        location,
        ErrorCode.NON_EXHAUSTIVE_MATCH,
        "Match on group '{s}' is not exhaustive: '{s}' not covered. Cover every member or add an 'else' arm",
        .{ group.name, listed },
    );
    self.fatal_error = true;
}

const EnumCoverage = struct {
    variants: []const Types.EnumVariant,
    covered: []bool,
};

const MemberCoverage = struct {
    whole: bool = false,
    /// Present only for an enum member whose declared variants were readable;
    /// a member in this state can also be covered variant by variant.
    enum_coverage: ?EnumCoverage = null,
};

/// The group a match subject's type names, when it names one.
pub fn matchSubjectGroup(self: *const SemanticAnalyzer, subject_type: *const ast.TypeInfo) ?TypeRef {
    return groupOf(self, subject_type);
}

fn matchHasElseArm(match_expr: ast.MatchExpr) bool {
    for (match_expr.cases) |case| {
        for (case.patterns) |pattern| {
            if (pattern.type == .ELSE or std.mem.eql(u8, pattern.lexeme, "else")) return true;
        }
    }
    return false;
}

fn groupMemberIndexOf(members: []const GroupTable.Member, qualifier: []const u8) ?usize {
    for (members, 0..) |member, index| {
        if (std.mem.eql(u8, member.qualifier, qualifier)) return index;
    }
    return null;
}

fn markVariantCovered(member_coverage: *MemberCoverage, variant_name: []const u8) void {
    const enum_coverage = member_coverage.enum_coverage orelse return;
    for (enum_coverage.variants, 0..) |variant, index| {
        if (std.mem.eql(u8, variant.name, variant_name)) enum_coverage.covered[index] = true;
    }
}

fn isMemberCovered(member_coverage: MemberCoverage) bool {
    if (member_coverage.whole) return true;
    const enum_coverage = member_coverage.enum_coverage orelse return false;
    for (enum_coverage.covered) |covered| {
        if (!covered) return false;
    }
    return true;
}

/// Order two named types deterministically: by defining record, then name.
fn refLessThan(a: TypeRef, b: TypeRef) bool {
    if (a.module != b.module) return a.module < b.module;
    return std.mem.lessThan(u8, a.name, b.name);
}

/// Canonicalize a slice of *TypeInfo (dedup + stable order).
fn canonicalizeUnion(
    self: *const SemanticAnalyzer,
    allocator: std.mem.Allocator,
    members: []const *ast.TypeInfo,
) ![]*ast.TypeInfo {
    var list: std.ArrayListUnmanaged(*ast.TypeInfo) = .empty;
    defer list.deinit(allocator);

    outer: for (members) |m| {
        for (list.items) |e| if (typesEqual(self, e, m)) continue :outer;
        try list.append(allocator, m);
    }

    std.sort.pdq(*ast.TypeInfo, list.items, {}, struct {
        fn lessThan(_: void, a: *ast.TypeInfo, b: *ast.TypeInfo) bool {
            const ba = @intFromEnum(a.base);
            const bb = @intFromEnum(b.base);
            if (ba != bb) return ba < bb;

            switch (a.base) {
                .Enum, .Custom => {
                    const ra = refOf(a) orelse return false;
                    const rb = refOf(b) orelse return false;
                    return refLessThan(ra, rb);
                },
                .Array => {
                    if (a.array_type == null or b.array_type == null) return false;
                    // Keeps it deterministic: compare base of element.
                    return @intFromEnum(a.array_type.?.base) < @intFromEnum(b.array_type.?.base);
                },
                else => return false,
            }
        }
    }.lessThan);

    return try list.toOwnedSlice(allocator);
}

/// The kind of declaration a named type is.
fn declKind(self: *const SemanticAnalyzer, ref: TypeRef) ?Types.CustomTypeKind {
    const decl = self.graph.declOf(.{ .module = ref.module, .name = ref.name, .kind = .Type }) orelse return null;
    return switch (decl) {
        .@"struct" => .Struct,
        .@"enum" => .Enum,
        .group => .Group,
        else => null,
    };
}

/// The struct id of a named struct type. Ids are allocated on first sight, so
/// a struct whose module's types are not registered yet already has its final
/// id.
pub fn structIdFromTypeInfo(self: *SemanticAnalyzer, ti: *const ast.TypeInfo) !?StructId {
    if (ti.base != .Struct and ti.base != .Custom) return null;
    const ref = refOf(ti) orelse return null;
    if (declKind(self, ref) != .Struct) return null;
    return try self.struct_table.idFor(ref);
}

/// Whether an array in `t` still lacks the element type an empty literal takes
/// from its context: `[]`, or `[[]]` at any depth.
pub fn hasUninferredElement(t: *const ast.TypeInfo) bool {
    if (t.base != .Array) return false;
    return hasUninferredElement(t.array_type orelse return true);
}

pub fn flattenUnionType(self: *SemanticAnalyzer, union_type: *ast.UnionType) !*ast.UnionType {
    var scratch: std.ArrayListUnmanaged(*ast.TypeInfo) = .empty;
    defer scratch.deinit(self.allocator);

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

    const unique = try canonicalizeUnion(self, self.allocator, scratch.items);

    const flattened = try self.allocator.create(ast.UnionType);
    flattened.* = .{
        .types = unique,
        .current_type_index = null, // order changed; don't carry index
    };
    return flattened;
}

pub fn createUnionType(self: *SemanticAnalyzer, types: []*ast.TypeInfo) !*ast.TypeInfo {
    var flat: std.ArrayListUnmanaged(*ast.TypeInfo) = .empty;
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
    if (unique.len == 1) return unique[0];

    const ut = try self.allocator.create(ast.UnionType);
    ut.* = .{ .types = unique, .current_type_index = null };

    const out = try ast.TypeInfo.createDefault(self.allocator);
    out.* = .{ .base = .Union, .union_type = ut, .is_mutable = false };
    return out;
}

pub fn unionContainsNothing(self: *SemanticAnalyzer, union_type_info: ast.TypeInfo) bool {
    _ = self;
    if (union_type_info.base != .Union) return false;
    if (union_type_info.union_type) |u| {
        for (u.types) |mt| if (mt.base == .Nothing) return true;
    }
    return false;
}

pub fn subtractTypeFromUnion(self: *SemanticAnalyzer, union_type_info: *const ast.TypeInfo, target: *const ast.TypeInfo) !*ast.TypeInfo {
    if (union_type_info.base != .Union or union_type_info.union_type == null) {
        const copy = try ast.TypeInfo.createDefault(self.allocator);
        copy.* = union_type_info.*;
        return copy;
    }
    const u = union_type_info.union_type.?;
    var remaining: std.ArrayListUnmanaged(*ast.TypeInfo) = .empty;
    defer remaining.deinit(self.allocator);
    for (u.types) |member| {
        if (!typesEqual(self, target, member)) try remaining.append(self.allocator, member);
    }
    if (remaining.items.len == 0) {
        const nothing = try ast.TypeInfo.createDefault(self.allocator);
        nothing.* = .{ .base = .Nothing };
        return nothing;
    }
    if (remaining.items.len == 1) return remaining.items[0];
    return try createUnionType(self, remaining.items);
}

/// An array literal is typed by the array its context expects, as an integer
/// literal is by the number type: each element is checked against the
/// expected element type (a nested literal in turn), and the literal takes
/// that element type and storage. So `[]` under `int[]` is an `int[]`,
/// `[65, 66]` under `byte[]` holds bytes, and a literal initializing an
/// `int[3]` is laid out fixed — codegen reads all of it from the literal.
fn contextualizeArrayLiteral(self: *SemanticAnalyzer, expected: *const ast.TypeInfo, actual: *ast.TypeInfo, elements: []const *ast.Expr, span: ast.SourceSpan) infer_type.SemanticError!void {
    if (expected.array_type) |element_type| {
        for (elements) |element| {
            const element_actual = try infer_type.inferTypeFromExpr(self, element);
            if (!adoptByteLiteral(self, element, element_actual, element_type)) return;
            try unifyTypesExpr(self, element_type, element_actual, element, span);
        }
    }
    actual.array_type = expected.array_type orelse actual.array_type;
    actual.array_storage = expected.array_storage;
    actual.array_size = expected.array_size;
}

/// Make the integer literal `literal`, of type `literal_type`, a byte literal
/// when its context is a byte: the other operand of its arithmetic, or the
/// element type of the array it sits in. Returns false after reporting a
/// literal no byte can hold.
pub fn adoptByteLiteral(self: *SemanticAnalyzer, literal: *ast.Expr, literal_type: *ast.TypeInfo, context: *const ast.TypeInfo) bool {
    if (context.base != .Byte or literal_type.base != .Int) return true;
    const value = literal_type.comptime_int orelse return true;
    if (value < 0 or value > 255) {
        self.reporter.reportCompileError(literal.base.location(), ErrorCode.BYTE_VALUE_OUT_OF_RANGE, "byte value out of range (must be 0-255)", .{});
        self.fatal_error = true;
        return false;
    }
    if (literal.data != .Literal) return true;
    literal.data.Literal = .{ .byte = @intCast(value) };
    literal_type.base = .Byte;
    return true;
}

/// Check a value stored into an array element (`xs[i] is v`, `@push`,
/// `@insert`) against the element type. An element store is an operator
/// position, where a runtime number widens; an array literal stored there is
/// still typed by the element type it lands in.
pub fn unifyElement(self: *SemanticAnalyzer, element_type: *const ast.TypeInfo, value_type: *ast.TypeInfo, value: *ast.Expr, span: ast.SourceSpan) !void {
    contextualizeEnumMember(self, value, value_type, element_type);
    try unifyTypesExpr(self, element_type, value_type, if (value.data == .Array) value else null, span);
}

pub fn unifyTypes(self: *SemanticAnalyzer, expected: *const ast.TypeInfo, actual: *ast.TypeInfo, span: ast.SourceSpan) !void {
    return unifyTypesExpr(self, expected, actual, null, span);
}

pub fn unifyTypesExpr(self: *SemanticAnalyzer, expected: *const ast.TypeInfo, actual: *ast.TypeInfo, actual_expr: ?*ast.Expr, span: ast.SourceSpan) !void {
    if (actual_expr) |expr| contextualizeEnumMember(self, expr, actual, expected);

    // Two shorthands with no context yet agree; their context types both.
    if (isUntypedVariant(expected) and isUntypedVariant(actual)) return;
    if (actual_expr) |expr| if (reportUndeclaredVariant(self, expr, actual, expected, span)) return;
    if (actual_expr) |expr| if (expr.data == .Array and expected.base == .Array) {
        return contextualizeArrayLiteral(self, expected, actual, expr.data.Array, span);
    };

    // ── Phase 1: Group widening ──
    // If expected is a group, any member type (or union of members) is assignable.
    if (groupOf(self, expected)) |group| {
        if (typeWidensToGroup(self, group, actual)) return;
        if (actual.base == .Union) {
            if (actual.union_type) |act_u| {
                for (act_u.types) |act_m| {
                    if (!typeWidensToGroup(self, group, act_m)) break;
                } else return;
            }
        }
    }

    // ── Phase 2: Expected is Union ──
    // Actual must be a member of the expected union (or a subset if actual is also a union).
    if (expected.base == .Union) {
        if (expected.union_type) |exp_u| {
            if (actual.base == .Union) {
                if (actual.union_type) |act_u| {
                    for (act_u.types) |act_m| {
                        for (exp_u.types) |exp_m| {
                            if (typeMatchesUnionMember(self, exp_m, act_m)) break;
                        } else {
                            const listed = try unionLabel(self, exp_u);
                            defer self.allocator.free(listed);
                            self.reporter.reportCompileError(
                                span.location,
                                ErrorCode.TYPE_MISMATCH,
                                "Type mismatch: expected union ({s}), got member of kind {s}",
                                .{ listed, typeLabel(act_m) },
                            );
                            self.fatal_error = true;
                            return;
                        }
                    }
                    return;
                }
            } else {
                for (exp_u.types) |m| {
                    if (typeMatchesUnionMember(self, m, actual)) return;
                }
                const listed = try unionLabel(self, exp_u);
                defer self.allocator.free(listed);
                self.reporter.reportCompileError(
                    span.location,
                    ErrorCode.TYPE_MISMATCH,
                    "Type mismatch: expected union ({s}), got {s}",
                    .{ listed, typeLabel(actual) },
                );
                self.fatal_error = true;
                return;
            }
        }
    }

    // ── Phase 3: Actual is Union, expected is NOT Union ──
    // Unions must be narrowed with 'as' or 'match' before assignment to a non-union type.
    if (actual.base == .Union) {
        self.reporter.reportCompileError(
            span.location,
            ErrorCode.TYPE_MISMATCH,
            "{s} is not assignable to type {s}; use 'as' or 'match' to narrow the union",
            try typeLabels(self, actual, expected),
        );
        self.fatal_error = true;
        return;
    }

    // ── Phase 4: Non-union structural equality + implicit conversions ──
    if (!typesEqual(self, expected, actual)) {
        // Widening int/byte -> float: implicit only for comptime numeric literals
        // in value positions (where actual_expr is provided). Operator-like positions
        // (index assignments, binary ops) allow it unconditionally.
        if (expected.base == .Float and (actual.base == .Int or actual.base == .Byte)) {
            if (actual_expr) |expr| {
                if (actual.comptime_int) |lit_val| {
                    // A literal widens only when the float holds it exactly:
                    // past 2^53 an int rounds, and a literal that would
                    // silently change value is an error, not a conversion.
                    const widened: f64 = @floatFromInt(lit_val);
                    if (@as(i128, @intFromFloat(widened)) != lit_val) {
                        self.reporter.reportCompileError(
                            span.location,
                            ErrorCode.INEXACT_FLOAT_LITERAL,
                            "int literal {d} has no exact float value (the nearest float is {d}.0); write it as a float literal or convert with @float()",
                            .{ lit_val, widened },
                        );
                        self.fatal_error = true;
                        return;
                    }
                    switch (expr.data) {
                        .Literal => {
                            expr.data.Literal = ast.TokenLiteral{ .float = widened };
                        },
                        .Unary => |unary| {
                            if (unary.operator.type == .MINUS) {
                                if (unary.right) |right| {
                                    switch (right.data) {
                                        .Literal => |lit| {
                                            const inner_val: f64 = switch (lit) {
                                                .int => |i| @floatFromInt(i),
                                                .byte => |b| @floatFromInt(b),
                                                else => @floatFromInt(lit_val),
                                            };
                                            right.data.Literal = ast.TokenLiteral{ .float = inner_val };
                                        },
                                        else => {},
                                    }
                                }
                            }
                        },
                        else => {},
                    }
                    actual.base = .Float;
                    return;
                }
                self.reporter.reportCompileError(
                    span.location,
                    ErrorCode.TYPE_MISMATCH,
                    "int is not implicitly assignable to float; use @float() to widen",
                    .{},
                );
                self.fatal_error = true;
                return;
            }
            return;
        }
        // Narrowing int -> byte is implicit only for a literal the byte holds.
        // Runtime ints must be narrowed explicitly with @byte().
        if (expected.base == .Byte and actual.base == .Int) {
            if (actual.comptime_int) |value| {
                if (value < 0 or value > 255) {
                    self.reporter.reportCompileError(span.location, ErrorCode.BYTE_VALUE_OUT_OF_RANGE, "byte value out of range (must be 0-255)", .{});
                    self.fatal_error = true;
                }
                return;
            }
            self.reporter.reportCompileError(
                span.location,
                ErrorCode.TYPE_MISMATCH,
                "int is not implicitly assignable to byte; use @byte() to narrow",
                .{},
            );
            self.fatal_error = true;
            return;
        }
        if (expected.base == .Int and actual.base == .Byte) return;

        // Arrays: recurse into element types so element-level implicit
        // conversions (e.g. Int literal -> Byte) apply to array literals.
        if (expected.base == .Array and actual.base == .Array) {
            if (expected.array_type) |e|
                if (actual.array_type) |a|
                    try unifyTypesExpr(self, e, a, null, span);
            return;
        }

        // A named type is the same type however it is spelled: an enum
        // reference or value can arrive as `.Enum` or `.Custom` with the same
        // identity, and the inline-Zig signature parser spells enums as
        // `.Enum`. Identity decides.
        const expected_named = expected.base == .Enum or expected.base == .Custom;
        const actual_named = actual.base == .Enum or actual.base == .Custom;
        if (expected_named and actual_named) {
            if (refOf(expected)) |er| if (refOf(actual)) |ar| if (er.eql(ar)) return;
        }

        self.reporter.reportCompileError(
            span.location,
            ErrorCode.TYPE_MISMATCH,
            "{s} is not assignable to type {s}",
            try typeLabels(self, actual, expected),
        );
        self.fatal_error = true;
        return;
    }

    // ── Phase 5: Structural recursion ──
    switch (expected.base) {
        .Array => {
            if (expected.array_type) |e|
                if (actual.array_type) |a|
                    try unifyTypesExpr(self, e, a, null, span);
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
                        try unifyTypesExpr(self, ef.type_info, af.type_info, null, span);
                    }
                }
            }
        },
        else => {},
    }
}

fn unionLabel(self: *SemanticAnalyzer, union_type: *const ast.UnionType) ![]u8 {
    var list: std.ArrayListUnmanaged(u8) = .empty;
    for (union_type.types, 0..) |m, i| {
        if (i > 0) try list.appendSlice(self.allocator, " | ");
        try list.appendSlice(self.allocator, typeLabel(m));
    }
    return list.toOwnedSlice(self.allocator);
}

/// Register the struct `ref` with its resolved fields: its description in
/// `custom_types` and its table entry, with each field's HIR type and nested
/// struct id (stable from first sight, so a field naming a struct of a module
/// not yet registered lowers to that struct's final id).
pub fn registerStructType(self: *SemanticAnalyzer, ref: TypeRef, fields: []const ast.StructFieldType) !void {
    const struct_fields = try self.allocator.alloc(StructField, fields.len);
    for (fields, 0..) |field, index| {
        struct_fields[index] = .{
            .name = field.name,
            .field_type_info = field.type_info,
            .index = @intCast(index),
            .is_public = field.is_public,
        };
    }
    try self.custom_types.put(ref, .{ .ref = ref, .kind = .Struct, .struct_fields = struct_fields });

    const table_inputs = try self.allocator.alloc(StructTable.FieldInput, fields.len);
    defer self.allocator.free(table_inputs);
    for (fields, 0..) |field, idx| {
        table_inputs[idx] = .{ .name = field.name, .type_info = field.type_info };
    }
    const struct_id = try self.struct_table.registerStruct(ref, table_inputs);

    for (fields, 0..) |field, i| {
        if (try structIdFromTypeInfo(self, field.type_info)) |nested_struct_id| {
            self.struct_table.setNestedStructId(struct_id, @intCast(i), nested_struct_id);
        }
    }
}

/// Lower every struct field's type for codegen. A field may name a type of a
/// record whose types register after its struct's — a group, whose members a
/// union field flattens — so this runs once every record is analyzed.
pub fn lowerStructFieldTypes(self: *SemanticAnalyzer) !void {
    for (self.struct_table.entries.items) |entry| {
        for (entry.fields) |*field| field.hir_type = try self.typeLowering().lower(field.type_info);
    }
}

pub fn registerEnumType(self: *SemanticAnalyzer, ref: TypeRef, variants: []const []const u8) !void {
    const enum_variants = try self.allocator.alloc(Types.EnumVariant, variants.len);
    for (variants, 0..) |variant_name, index| {
        enum_variants[index] = .{ .name = variant_name, .index = @intCast(index) };
    }
    try self.custom_types.put(ref, .{ .ref = ref, .kind = .Enum, .enum_variants = enum_variants });
    _ = try self.enum_table.registerEnum(ref, variants);
}

/// Register the group `ref`: each member path resolves, against the current
/// record, to the type it names, and nested groups are flattened into the
/// table's member list.
pub fn registerGroupType(self: *SemanticAnalyzer, ref: TypeRef, members: []const ast.GroupMember) !void {
    if (members.len == 0) {
        self.reporter.reportCompileError(null, ErrorCode.EXPECTED_EXPRESSION, "Group '{s}' must have at least one member", .{ref.name});
        self.fatal_error = true;
        return;
    }

    const sources = try self.allocator.alloc(Types.GroupMemberSource, members.len);
    for (members, sources) |member, *source| {
        const location = if (member.path.len > 0) ast.SourceSpan.fromToken(member.path[0]).location else null;
        const spelled = try qualifiedName(self.allocator, member.path);
        const member_ref = (try names.resolveTypeName(self, spelled, self.current_module)) orelse {
            self.reporter.reportCompileError(location, ErrorCode.UNKNOWN_TYPE, "Group member '{s}' is not a declared type", .{spelled});
            self.fatal_error = true;
            return error.UndefinedType;
        };
        source.* = .{ .qualifier = member.qualifier, .ref = member_ref };
    }
    try self.custom_types.put(ref, .{ .ref = ref, .kind = .Group, .group_members = sources });

    var flat: std.ArrayListUnmanaged(GroupTable.Member) = .empty;
    defer flat.deinit(self.allocator);
    var visited: std.ArrayListUnmanaged(TypeRef) = .empty;
    defer visited.deinit(self.allocator);
    try visited.append(self.allocator, ref);
    try flattenGroupMembers(self, sources, &flat, &visited);

    // A qualifier names one member after flattening.
    for (flat.items, 0..) |member, i| {
        for (flat.items[0..i]) |earlier| {
            if (!std.mem.eql(u8, earlier.qualifier, member.qualifier)) continue;
            self.reporter.reportCompileError(null, ErrorCode.TYPE_MISMATCH, "Group '{s}' has duplicate qualifier '{s}'", .{ ref.name, member.qualifier });
            self.fatal_error = true;
        }
    }

    _ = try self.group_table.registerGroup(ref, flat.items);
}

fn flattenGroupMembers(
    self: *SemanticAnalyzer,
    sources: []const Types.GroupMemberSource,
    flat: *std.ArrayListUnmanaged(GroupTable.Member),
    visited: *std.ArrayListUnmanaged(TypeRef),
) !void {
    for (sources) |source| {
        const kind = declKind(self, source.ref) orelse continue;
        const member_kind: GroupTable.MemberKind = switch (kind) {
            .Enum => .Enum,
            .Struct => .Struct,
            .Group => {
                for (visited.items) |seen| {
                    if (!seen.eql(source.ref)) continue;
                    self.reporter.reportCompileError(null, ErrorCode.TYPE_MISMATCH, "Cycle detected in group: '{s}' includes itself transitively", .{source.ref.name});
                    self.fatal_error = true;
                    break;
                } else {
                    try visited.append(self.allocator, source.ref);
                    const nested = (try self.customType(source.ref)) orelse continue;
                    try flattenGroupMembers(self, nested.group_members orelse &.{}, flat, visited);
                }
                continue;
            },
        };
        const id: u32 = switch (member_kind) {
            .Enum => try self.enum_table.idFor(source.ref),
            .Struct => try self.struct_table.idFor(source.ref),
            .Group => unreachable,
        };
        for (flat.items) |existing| {
            if (existing.ref.eql(source.ref)) break;
        } else try flat.append(self.allocator, .{ .qualifier = source.qualifier, .kind = member_kind, .ref = source.ref, .id = id });
    }
}

fn qualifiedName(allocator: std.mem.Allocator, path: []const ast.Token) ![]const u8 {
    var out = std.array_list.Managed(u8).init(allocator);
    for (path, 0..) |token, i| {
        if (i > 0) try out.append('.');
        try out.appendSlice(token.lexeme);
    }
    return out.toOwnedSlice();
}

pub fn getLocationFromBase(base: ast.Base) Reporting.Location {
    return base.location();
}
