const std = @import("std");
const ast = @import("../../../ast/ast.zig");
const types = @import("../../../types/types.zig");
const HIRGenerator = @import("../soxa_generator.zig").HIRGenerator;
const HIRValue = @import("../soxa_values.zig").HIRValue;
const Slot = @import("../soxa_types.zig").Slot;
const HIRType = @import("../soxa_types.zig").HIRType;
const StructId = @import("../soxa_types.zig").StructId;
const EnumId = @import("../soxa_types.zig").EnumId;
const TypeSystem = @import("../type_system.zig").TypeSystem;
const HIREnum = @import("../soxa_values.zig").HIREnum;
const HIRInstruction = @import("../soxa_instructions.zig").HIRInstruction;
const Location = @import("../../../utils/reporting.zig").Location;
const ErrorCode = @import("../../../utils/errors.zig").ErrorCode;
const ErrorList = @import("../../../utils/errors.zig").ErrorList;
const TETRA_TRUE = @import("../soxa_generator.zig").TETRA_TRUE;
const generateStatement = @import("../soxa_statements.zig").generateStatement;
const GroupId = @import("../soxa_types.zig").GroupId;
const GroupTable = @import("../../../common/group_table.zig").GroupTable;
const DoxaTag = @import("../../../runtime/doxa_rt.zig").DoxaTag;
const graph = @import("../../../module/graph.zig");

/// Whether `ty` is the named type `named`: the same struct, enum or group.
fn isNamedType(ty: HIRType, named: HIRType) bool {
    return switch (named) {
        .Struct => |id| ty == .Struct and ty.Struct == id,
        .Enum => |id| ty == .Enum and ty.Enum == id,
        .Group => |id| ty == .Group and ty.Group == id,
        else => false,
    };
}

/// The type a group's flattened member names.
fn groupMemberType(member: GroupTable.Member) HIRType {
    return switch (member.kind) {
        .Enum => .{ .Enum = member.id },
        .Struct => .{ .Struct = member.id },
        .Group => .{ .Group = member.id },
    };
}

/// Handle control flow expressions: if, match, loops, blocks
pub const ControlFlowHandler = struct {
    generator: *HIRGenerator,

    const PatternLiteralLowering = struct {
        value: HIRValue,
        operand_type: HIRType,
    };

    pub fn init(generator: *HIRGenerator) ControlFlowHandler {
        return .{ .generator = generator };
    }

    /// A group type the match subject belongs to: arms are resolved against its
    /// flattened member list rather than against an enum. `name` is the
    /// group's declared name, which a path may be written through.
    const MatchGroup = struct {
        id: GroupId,
        name: []const u8,
    };

    /// Jump targets available while emitting the checks of a single `match` case.
    const MatchTargets = struct {
        case_labels: []const []const u8,
        check_labels: []const []const u8,
        end_label: []const u8,
        fail_label: ?[]const u8,
        case_count: usize,
    };

    fn caseBodyLabel(targets: MatchTargets, case_idx: usize) []const u8 {
        return targets.case_labels[case_idx];
    }

    /// Where control goes when `case_idx` does not match: the next case's check
    /// label, or the match's failure path once the last case is exhausted.
    fn nextCaseLabel(targets: MatchTargets, case_idx: usize) []const u8 {
        if (case_idx + 1 < targets.case_count) return targets.check_labels[case_idx];
        if (targets.fail_label) |fail_label| return fail_label;
        return targets.end_label;
    }

    fn isElsePattern(pattern: ast.Token) bool {
        return pattern.type == .ELSE or std.mem.eql(u8, pattern.lexeme, "else");
    }

    /// The group the analyzer typed `subject` as, or null when it is not
    /// group-typed.
    fn resolveMatchGroup(self: *ControlFlowHandler, subject: *ast.Expr) ?MatchGroup {
        const type_info = self.generator.semantic.getCachedExprType(subject) orelse return null;
        const custom = type_info.custom_type orelse return null;
        const ref = custom.resolved();
        const id = self.generator.semantic.group_table.idOf(ref) orelse return null;
        return .{ .id = id, .name = ref.name };
    }

    const GroupMember = struct { index: u32, key: []const u8 };

    /// Locate the member `qualifier` names among `group`'s flattened members.
    fn groupMember(self: *ControlFlowHandler, group: MatchGroup, qualifier: ast.Token) ErrorList!GroupMember {
        const members = self.generator.semantic.group_table.members(group.id) orelse &.{};
        for (members, 0..) |member, idx| {
            if (std.mem.eql(u8, member.qualifier, qualifier.lexeme)) {
                return .{ .index = @intCast(idx), .key = self.generator.typeKey(member.ref) };
            }
        }
        self.generator.reporter.reportCompileError(
            tokenLocation(qualifier),
            ErrorCode.TYPE_MISMATCH,
            "'{s}' is not a member of group '{s}'",
            .{ qualifier.lexeme, group.name },
        );
        return ErrorList.TypeMismatch;
    }

    /// The member type a box of type `boxed` holds at `index`: a union's
    /// member, or a group's flattened member.
    fn boxMember(self: *ControlFlowHandler, boxed: HIRType, index: usize) HIRType {
        return switch (boxed) {
            .Union => |u| u.members[index].*,
            .Group => |gid| groupMemberType(self.generator.semantic.group_table.members(gid).?[index]),
            else => unreachable,
        };
    }

    fn boxMemberCount(self: *ControlFlowHandler, boxed: HIRType) usize {
        return switch (boxed) {
            .Union => |u| u.members.len,
            .Group => |gid| if (self.generator.semantic.group_table.members(gid)) |members| members.len else 0,
            else => 0,
        };
    }

    /// Whether a box member of type `member` is a value of `named`: the named
    /// type itself, or, for a group, one of the members it flattened into.
    fn memberIsNamed(self: *ControlFlowHandler, member: HIRType, named: HIRType) bool {
        if (isNamedType(member, named)) return true;
        if (named != .Group) return false;
        const group_members = self.generator.semantic.group_table.members(named.Group) orelse return false;
        for (group_members) |group_member| {
            if (isNamedType(member, groupMemberType(group_member))) return true;
        }
        return false;
    }

    /// The indexes of every member of box type `boxed` that is a `named`.
    fn boxedMembersNamed(self: *ControlFlowHandler, boxed: HIRType, named: HIRType) ![]const u32 {
        var indices: std.ArrayListUnmanaged(u32) = .empty;
        for (0..self.boxMemberCount(boxed)) |idx| {
            if (self.memberIsNamed(self.boxMember(boxed, idx), named)) try indices.append(self.generator.allocator, @intCast(idx));
        }
        return indices.toOwnedSlice(self.generator.allocator);
    }

    /// Emit the member check for one arm: the subject must be boxed as a group
    /// value carrying `member_index`.
    fn emitGroupMemberCheck(self: *ControlFlowHandler, member_index: u32, body_label: []const u8, fail_label: []const u8) !void {
        try self.generator.instructions.append(.Dup);
        try self.generator.instructions.append(.{ .MemberCheck = .{ .members = try self.generator.allocator.dupe(u32, &.{member_index}) } });
        try self.generator.instructions.append(.{ .JumpCond = .{
            .label_true = body_label,
            .label_false = fail_label,
            .condition_type = .Tetra,
        } });
    }

    /// Emit the payload compare that narrows an enum member's arm to a single
    /// variant (`IOError.NotFound` matching only `NotFound`).
    fn emitGroupVariantCheck(self: *ControlFlowHandler, member_key: []const u8, variant: ast.Token, body_label: []const u8, fail_label: []const u8) !void {
        const enum_id = self.generator.semantic.enum_table.idByKey(member_key) orelse {
            self.generator.reporter.reportCompileError(
                tokenLocation(variant),
                ErrorCode.TYPE_MISMATCH,
                "'{s}' is not an enum type",
                .{graph.displayName(member_key)},
            );
            return ErrorList.TypeMismatch;
        };
        const pattern_value = try self.enumVariantValue(enum_id, variant);

        // The group box is stripped first so the compare sees the member's own
        // discriminant, not the wrapped %DoxaValue.
        try self.generator.instructions.append(.Dup);
        try self.generator.instructions.append(.{ .UnboxPayload = .{} });

        const pattern_idx = try self.generator.addConstant(pattern_value);
        try self.generator.instructions.append(.{ .Const = .{ .value = pattern_value, .constant_id = pattern_idx } });
        try self.generator.instructions.append(.{ .Compare = .{ .op = .Eq, .operand_type = HIRType{ .Enum = enum_id } } });
        try self.generator.instructions.append(.{ .JumpCond = .{
            .label_true = body_label,
            .label_false = fail_label,
            .condition_type = .Tetra,
        } });
    }

    /// Emit every check for one `match` case against a group subject, OR-ing the
    /// case's patterns and AND-ing the member check with the variant check of a
    /// single pattern. Returns false for an `else` arm, which the generic
    /// pattern loop still has to handle.
    fn emitGroupCaseChecks(self: *ControlFlowHandler, case: ast.MatchCase, case_idx: usize, group: MatchGroup, targets: MatchTargets) ErrorList!bool {
        const body_label = caseBodyLabel(targets, case_idx);
        const last_case_fail = nextCaseLabel(targets, case_idx);

        if (case.path_patterns.len > 0) {
            for (case.path_patterns) |path_pattern| {
                if (path_pattern.tokens.len == 0) return false;
            }
            for (case.path_patterns, 0..) |path_pattern, path_idx| {
                const fail_label = if (path_idx + 1 < case.path_patterns.len)
                    try self.generator.generateLabel("next_group_pattern")
                else
                    last_case_fail;
                try self.emitGroupPathChecks(path_pattern, group, body_label, fail_label);
                if (path_idx + 1 < case.path_patterns.len) {
                    try self.generator.instructions.append(.{ .Label = .{ .name = fail_label } });
                }
            }
            return true;
        }

        if (case.patterns.len == 0) return false;
        for (case.patterns) |pattern| {
            if (isElsePattern(pattern)) return false;
        }

        for (case.patterns, 0..) |pattern, pattern_idx| {
            const fail_label = if (pattern_idx + 1 < case.patterns.len)
                try self.generator.generateLabel("next_group_pattern")
            else
                last_case_fail;
            const member = try self.groupMember(group, pattern);
            try self.emitGroupMemberCheck(member.index, body_label, fail_label);
            if (pattern_idx + 1 < case.patterns.len) {
                try self.generator.instructions.append(.{ .Label = .{ .name = fail_label } });
            }
        }
        return true;
    }

    fn emitGroupPathChecks(self: *ControlFlowHandler, path_pattern: ast.MatchCase.PathPattern, group: MatchGroup, body_label: []const u8, fail_label: []const u8) !void {
        if (path_pattern.tokens.len == 0) return;
        const split = path_pattern.split(group.name);
        const member = try self.groupMember(group, split.member);

        // A dotted path that is not a domain wildcard carries a variant, and the
        // variant decides the arm: the member check must fall through into it
        // rather than jump straight to the body and strand it unreachable.
        const narrows = !path_pattern.is_wildcard and split.variant != null;
        const after_member = if (narrows)
            try self.generator.generateLabel("group_variant")
        else
            body_label;
        try self.emitGroupMemberCheck(member.index, after_member, fail_label);

        if (!narrows) return;
        try self.generator.instructions.append(.{ .Label = .{ .name = after_member } });
        try self.emitGroupVariantCheck(member.key, split.variant.?, body_label, fail_label);
    }

    fn tokenLocation(token: ast.Token) Location {
        return Location{
            .file = token.file,
            .file_uri = token.file_uri,
            .range = .{
                .start_line = token.line,
                .start_col = token.column,
                .end_line = token.line,
                .end_col = token.column + token.lexeme.len,
            },
        };
    }

    fn typeNeedsRuntimeScope(info: ast.TypeInfo) bool {
        return switch (info.base) {
            .String, .Array, .Struct, .Map, .Union => true,
            else => false,
        };
    }

    fn expressionNeedsRuntimeScope(self: *ControlFlowHandler, expr: *ast.Expr) bool {
        if (self.generator.semantic.getCachedExprType(expr)) |info| {
            if (typeNeedsRuntimeScope(info.*)) return true;
        }
        return switch (expr.data) {
            .Literal => |literal| switch (literal) {
                .string => true,
                else => false,
            },
            .InterpolatedString, .Array, .Struct, .StructLiteral, .Input => true,
            // Match lowering can allocate into the current arena: a subject that
            // is a string index is materialised as a 1-char heap string via
            // `doxa_char_to_string`, and string pattern comparisons may build
            // temporaries. Treat every match as arena-live.
            .Match => true,
            .Block => |block| self.blockNeedsRuntimeScope(block.statements, block.value),
            .If => |if_expr| (if_expr.then_branch != null and self.expressionNeedsRuntimeScope(if_expr.then_branch.?)) or
                (if_expr.else_branch != null and self.expressionNeedsRuntimeScope(if_expr.else_branch.?)),
            else => false,
        };
    }

    fn blockNeedsRuntimeScope(self: *ControlFlowHandler, statements: []ast.Stmt, value: ?*ast.Expr) bool {
        for (statements) |statement| {
            switch (statement.data) {
                .VarDecl => |decl| {
                    if (typeNeedsRuntimeScope(decl.type_info)) return true;
                    if (decl.initializer) |initializer| {
                        if (self.expressionNeedsRuntimeScope(initializer)) return true;
                    }
                },
                .Expression => |expr| {
                    if (expr) |e| if (self.expressionNeedsRuntimeScope(e)) return true;
                },
                .Return => |ret| {
                    if (ret.value) |e| if (self.expressionNeedsRuntimeScope(e)) return true;
                },
                else => {},
            }
        }
        return if (value) |e| self.expressionNeedsRuntimeScope(e) else false;
    }

    fn lowerMatchPatternLiteral(literal: ast.TokenLiteral) PatternLiteralLowering {
        return switch (literal) {
            .int => |v| .{ .value = .{ .int = v }, .operand_type = .Int },
            .byte => |v| .{ .value = .{ .byte = v }, .operand_type = .Byte },
            .float => |v| .{ .value = .{ .float = v }, .operand_type = .Float },
            .string => |v| .{ .value = .{ .string = v }, .operand_type = .String },
            .tetra => |v| .{ .value = .{ .tetra = HIRGenerator.tetraFromEnum(v) }, .operand_type = .Tetra },
            .nothing => .{ .value = .{ .nothing = .{} }, .operand_type = .Nothing },
            else => .{ .value = .{ .string = "" }, .operand_type = .String },
        };
    }

    /// Generate HIR for if expressions
    pub fn generateIf(self: *ControlFlowHandler, expr: *ast.Expr, preserve_result: bool, should_pop_after_use: bool) (std.mem.Allocator.Error || ErrorList)!void {
        const if_expr = expr.data.If;
        // Special-case: if inside a loop and the then/else branch is a pure break/continue block,
        // emit a direct conditional jump to the loop label (so control flow skips subsequent body code).
        var handled_as_loop_control = false;
        const lc_opt = self.generator.currentLoopContext();

        // Helper lambdas for detection
        const isControlOnlyBlock = struct {
            fn run(_: *HIRGenerator, node: *ast.Expr, want_break: bool, want_continue: bool) bool {
                switch (node.data) {
                    .Block => |blk| {
                        if (blk.statements.len == 0) return false;
                        // Require all statements to be the desired control kind(s)
                        for (blk.statements) |s| {
                            const d = s.data;
                            if (want_break and d == .Break) continue;
                            if (want_continue and d == .Continue) continue;
                            // Allow empty expression statements as no-ops
                            if (d == .Expression and s.data.Expression == null) continue;
                            return false;
                        }
                        return true;
                    },
                    else => return false,
                }
            }
        };

        // Folding a branch down to a direct jump to the loop label emits *only*
        // that jump — every other branch is dropped on the floor. It is
        // therefore sound only when the `if` has no else branch to lose. With
        // an else branch present the condition's false edge leads somewhere
        // real, and that code has to be emitted.
        if (lc_opt) |lc| {
            if (if_expr.else_branch == null) {
                const then_is_continue = isControlOnlyBlock.run(self.generator, if_expr.then_branch.?, false, true);
                const then_is_break = isControlOnlyBlock.run(self.generator, if_expr.then_branch.?, true, false);

                if (then_is_continue and !then_is_break) {
                    // If TRUE -> continue label, else fall-through
                    try self.generator.generateExpression(if_expr.condition.?, true, should_pop_after_use);
                    const end_if = try self.generator.generateLabel("end_if");
                    try self.generator.instructions.append(.{ .JumpCond = .{ .label_true = lc.continue_label, .label_false = end_if, .condition_type = .Tetra } });
                    try self.generator.instructions.append(.{ .Label = .{ .name = end_if } });
                    handled_as_loop_control = true;
                } else if (then_is_break and !then_is_continue) {
                    // If TRUE -> break label, else fall-through
                    try self.generator.generateExpression(if_expr.condition.?, true, should_pop_after_use);
                    const end_if = try self.generator.generateLabel("end_if");
                    try self.generator.instructions.append(.{ .JumpCond = .{ .label_true = lc.break_label, .label_false = end_if, .condition_type = .Tetra } });
                    try self.generator.instructions.append(.{ .Label = .{ .name = end_if } });
                    handled_as_loop_control = true;
                }
            }
        }

        if (!handled_as_loop_control) {
            // Standard if codegen
            try self.generator.generateExpression(if_expr.condition.?, true, should_pop_after_use);

            const end_label = try self.generator.generateLabel("end_if");
            const then_label = try self.generator.generateLabel("then");

            if (if_expr.else_branch) |else_branch| {
                // Check if else branch is just a nothing literal (implicit else)
                const is_implicit_nothing = switch (else_branch.data) {
                    .Literal => |lit| switch (lit) {
                        .nothing => true,
                        else => false,
                    },
                    else => false,
                };

                if (is_implicit_nothing) {
                    // No real else branch - only generate then branch
                    try self.generator.instructions.append(.{
                        .JumpCond = .{
                            .label_true = then_label,
                            .label_false = end_label,
                            .condition_type = .Tetra,
                        },
                    });

                    // THEN branch
                    try self.generator.instructions.append(.{ .Label = .{ .name = then_label } });
                    if (preserve_result) {
                        try self.generator.generateExpression(if_expr.then_branch.?, true, should_pop_after_use);
                    } else {
                        // Statement context: do not produce a value
                        try self.generator.generateExpression(if_expr.then_branch.?, false, should_pop_after_use);
                    }
                } else {
                    // Has real else branch - generate both branches
                    const else_label = try self.generator.generateLabel("else");
                    try self.generator.instructions.append(.{
                        .JumpCond = .{
                            .label_true = then_label,
                            .label_false = else_label,
                            .condition_type = .Tetra,
                        },
                    });

                    // A value `if` whose branches are different types is a union
                    // of them: each branch boxes its value as that type, so the
                    // two merge as one box.
                    const result_box: ?HIRType = if (preserve_result) blk: {
                        const result_type = try self.generator.typeOf(expr);
                        break :blk if (result_type == .Union or result_type == .Group) result_type else null;
                    } else null;

                    // THEN branch
                    try self.generator.instructions.append(.{ .Label = .{ .name = then_label } });
                    if (preserve_result) {
                        try self.generator.generateExpression(if_expr.then_branch.?, true, should_pop_after_use);
                        if (result_box) |boxed_type| try self.generator.instructions.append(.{ .Box = .{ .boxed_type = boxed_type } });
                    } else {
                        // Statement context: do not produce a value
                        try self.generator.generateExpression(if_expr.then_branch.?, false, should_pop_after_use);
                    }
                    try self.generator.instructions.append(.{ .Jump = .{ .label = end_label } });

                    // ELSE branch
                    try self.generator.instructions.append(.{ .Label = .{ .name = else_label } });
                    if (preserve_result) {
                        try self.generator.generateExpression(if_expr.else_branch.?, true, should_pop_after_use);
                        if (result_box) |boxed_type| try self.generator.instructions.append(.{ .Box = .{ .boxed_type = boxed_type } });
                        // Terminate the value-bearing else arm with an explicit
                        // jump, mirroring the then arm. Without it the else path
                        // falls through into `end_label` and the emitter never
                        // records the else value for the merge, so the phi ends up
                        // with only the then arm (invalid IR: the then value does
                        // not dominate the merge). A branch that already diverged
                        // is skipped by the emitter's terminator guard. Statement
                        // contexts carry no value to merge and keep falling
                        // through, exactly as before.
                        try self.generator.instructions.append(.{ .Jump = .{ .label = end_label } });
                    } else {
                        try self.generator.generateExpression(if_expr.else_branch.?, false, should_pop_after_use);
                    }
                }
            } else {
                // No else branch - only generate then branch
                try self.generator.instructions.append(.{
                    .JumpCond = .{
                        .label_true = then_label,
                        .label_false = end_label,
                        .condition_type = .Tetra,
                    },
                });

                // THEN branch
                try self.generator.instructions.append(.{ .Label = .{ .name = then_label } });
                if (preserve_result) {
                    // If we need to preserve result but there's no else branch,
                    // we need to generate a nothing value for the else case
                    try self.generator.generateExpression(if_expr.then_branch.?, true, should_pop_after_use);
                    // Jump to end to skip the nothing value generation
                    try self.generator.instructions.append(.{ .Jump = .{ .label = end_label } });
                } else {
                    // Statement context: do not produce a value
                    try self.generator.generateExpression(if_expr.then_branch.?, false, should_pop_after_use);
                }
            }
            try self.generator.instructions.append(.{ .Label = .{ .name = end_label } });
        }
    }

    /// The struct a match subject is, when the analyzer typed it as one.
    fn matchSubjectStructId(subject_type: HIRType) ?StructId {
        return if (subject_type == .Struct and subject_type.Struct != 0) subject_type.Struct else null;
    }

    /// When both the match subject and a pattern name struct types, the arm is
    /// decided statically: `true` when the subject is that struct, `false` when
    /// it is a different one. Null leaves the pattern to the checks below.
    fn structTypePatternMatches(self: *ControlFlowHandler, subject_type: HIRType, resolved: ast.MatchCase.Resolved) ?bool {
        const subject_sid = matchSubjectStructId(subject_type) orelse return null;
        const named = switch (resolved) {
            .type => |ref| self.generator.type_system.typeForRef(ref),
            .token, .variant => return null,
        };
        if (named != .Struct) return null;
        return subject_sid == named.Struct;
    }

    /// How a custom-type pattern resolves against a union subject. A union
    /// boxes the active member's index in `reserved` — the same box a group
    /// carries — so the arm is one bit test against that index.
    const UnionPattern = union(enum) {
        /// The subject's union boxes this type at `index`.
        member: struct {
            /// The members that are the pattern's type: one, or every member
            /// a group pattern flattened into.
            indices: []const u32,
            /// The struct a destructuring pattern binds its fields from.
            struct_id: ?StructId,
            /// Runtime tag the boxed member must carry, when it is unambiguous
            /// (struct vs enum). Null for a group member, whose boxed tag is its
            /// underlying member's.
            tag: ?u32 = null,
        },
        /// The union never boxes this type, so the arm can never run.
        never,
    };

    /// Resolve a pattern against the union the subject is typed as. Null when
    /// the subject is not a union or the pattern names no type — both are
    /// owned by the checks below.
    fn unionPatternFor(self: *ControlFlowHandler, subject_type: HIRType, resolved: ast.MatchCase.Resolved) !?UnionPattern {
        if (subject_type != .Union) return null;
        const named = switch (resolved) {
            .type => |ref| self.generator.type_system.typeForRef(ref),
            .token, .variant => return null,
        };

        const indices = try self.boxedMembersNamed(subject_type, named);
        if (indices.len == 0) return .never;
        return .{ .member = switch (named) {
            .Struct => |sid| .{ .indices = indices, .struct_id = sid, .tag = @intFromEnum(DoxaTag.Struct) },
            .Enum => .{ .indices = indices, .struct_id = null, .tag = @intFromEnum(DoxaTag.Enum) },
            else => .{ .indices = indices, .struct_id = null },
        } };
    }

    /// What a type test asks for: a builtin type by its display name
    /// (`int`, `string[]`), or a named type by identity.
    const TypeTest = union(enum) {
        builtin: []const u8,
        named: HIRType,
    };

    /// Whether the union member `member` is what `type_test` asks for.
    fn memberPasses(self: *ControlFlowHandler, member: HIRType, type_test: TypeTest) !bool {
        return switch (type_test) {
            .named => |named| self.memberIsNamed(member, named),
            .builtin => |name| std.mem.eql(u8, try self.generator.hirTypeToDisplayName(member), name),
        };
    }

    /// What a match pattern says the subject is once the arm runs: the member
    /// type a type pattern (`int`, `string`, `int[]`) or a named-type pattern
    /// (`FileError`) names. Null for a pattern that selects a value rather than
    /// a type — `else`, an enum variant.
    fn patternTypeTest(self: *ControlFlowHandler, pattern: ast.Token, resolved: ast.MatchCase.Resolved) ?TypeTest {
        switch (resolved) {
            .type => |ref| return .{ .named = self.generator.type_system.typeForRef(ref) },
            .variant => return null,
            .token => {},
        }
        if (std.mem.indexOf(u8, pattern.lexeme, "[]") != null) return .{ .builtin = pattern.lexeme };
        return .{ .builtin = switch (pattern.type) {
            .INT_TYPE, .INT => "int",
            .FLOAT_TYPE, .FLOAT => "float",
            .STRING_TYPE, .STRING => "string",
            .BYTE_TYPE, .BYTE => "byte",
            .TETRA_TYPE, .TETRA => "tetra",
            .NOTHING_TYPE, .NOTHING => "nothing",
            else => return null,
        } };
    }

    /// The single-member union view an arm narrows a union-typed subject to, or
    /// null when narrowing would not be true of every way the arm can be
    /// selected: `else` names no member (the box holds any of them), a
    /// multi-pattern arm may be picked by patterns that disagree, and a
    /// pattern the union cannot hold is a check that always fails.
    ///
    /// The jump into the arm body is exactly the evidence the view asserts — a
    /// `MemberCheck`/`TypeCheck` that passed for this one pattern — so a load
    /// of the subject inside the body unwraps the box to that member and
    /// infers as it, the same contract `as` narrowing relies on.
    fn armNarrowingView(self: *ControlFlowHandler, subject: *ast.Expr, case: ast.MatchCase) !?HIRType {
        if (subject.data != .Variable) return null;
        if (case.patterns.len != 1) return null;
        const var_name = subject.data.Variable.lexeme;
        const saved_type = self.generator.getTrackedVariableType(var_name) orelse return null;
        if (saved_type != .Union) return null;

        const type_test = self.patternTypeTest(case.patterns[0], case.resolved[0]) orelse return null;
        for (saved_type.Union.members) |member_ptr| {
            if (!try self.memberPasses(member_ptr.*, type_test)) continue;
            const view_members = try self.generator.allocator.alloc(*const HIRType, 1);
            view_members[0] = try self.narrowedMember(member_ptr, type_test);
            return HIRType{ .Union = .{ .id = saved_type.Union.id, .members = view_members } };
        }
        return null;
    }

    /// What a value narrowed by `type_test` is, given that the union member
    /// `member` passed it: that member, or for a group test the group itself —
    /// the union holds the group's members, and the narrowed value is boxed as
    /// the group's.
    fn narrowedMember(self: *ControlFlowHandler, member: *const HIRType, type_test: TypeTest) !*const HIRType {
        const group = switch (type_test) {
            .named => |named| if (named == .Group) named else return member,
            .builtin => return member,
        };
        const group_ptr = try self.generator.allocator.create(HIRType);
        group_ptr.* = group;
        return group_ptr;
    }

    /// The enum a pattern compares against as a variant, when it is one.
    fn patternEnum(self: *ControlFlowHandler, resolved: ast.MatchCase.Resolved) ?EnumId {
        return switch (resolved) {
            .variant => |ref| self.generator.semantic.enum_table.idOf(ref),
            .token, .type => null,
        };
    }

    /// A false-label for case `i`: the next case's check, the fail block, or
    /// the end of the match.
    fn caseFalseLabel(i: usize, case_count: usize, check_labels: []const []const u8, fail_label: ?[]const u8, end_label: []const u8) []const u8 {
        if (i + 1 < case_count) return check_labels[i];
        return fail_label orelse end_label;
    }

    pub fn generateMatch(self: *ControlFlowHandler, expr: *ast.Expr, preserve_result: bool) ErrorList!void {
        const match_expr = expr.data.Match;
        // The subject's type is the analyzer's answer for the subject
        // expression, whatever its form: a local, a field, an element, an
        // `each` binding or a call result all match the same way.
        const subject_type = try self.generator.typeOf(match_expr.value);
        // A match whose arms are different types is a union of them (or a group
        // its context gave it): every arm boxes its value as that type, so the
        // arms merge as one box whatever member each produced.
        const result_box: ?HIRType = if (preserve_result) blk: {
            const result_type = try self.generator.typeOf(expr);
            break :blk if (result_type == .Union or result_type == .Group) result_type else null;
        } else null;

        // A group subject is discriminated by its boxed member index, never by
        // an enum variant index.
        const match_group = self.resolveMatchGroup(match_expr.value);

        // Track whether any pattern is an explicit else (wildcard) to know if falling through is possible.
        var has_else_case = false;
        for (match_expr.cases) |case| {
            for (case.patterns) |pattern| {
                if (isElsePattern(pattern)) {
                    has_else_case = true;
                    break;
                }
            }
            if (has_else_case) break;
        }

        try self.generator.generateExpression(match_expr.value, true, false);
        const fail_location = match_expr.value.base.location();

        // Create labels for each case body and the end
        const end_label = try self.generator.generateLabel("match_end");
        // Without an `else` arm the last check has to go somewhere: the fail
        // block drops the unmatched subject, then either traps — a value match
        // that produced nothing — or simply continues for a statement match.
        const fail_label = if (!has_else_case)
            try self.generator.generateLabel("match_fail")
        else
            null;
        var case_labels = std.array_list.Managed([]const u8).init(self.generator.allocator);
        defer case_labels.deinit();
        var check_labels = std.array_list.Managed([]const u8).init(self.generator.allocator);
        defer check_labels.deinit();

        // An arm the checks rule out statically gets no body at all: nothing
        // jumps to its label, so the emitter would run its field bindings off
        // an empty stack and record an empty merge state for `match_end`.
        var dead_cases = try self.generator.allocator.alloc(bool, match_expr.cases.len);
        defer self.generator.allocator.free(dead_cases);
        @memset(dead_cases, false);

        // Generate labels for each case body and case check
        for (match_expr.cases, 0..) |_, i| {
            const case_label = try self.generator.generateLabel("match_case");
            try case_labels.append(case_label);

            // Create check labels for all but the first case (first case starts immediately)
            if (i > 0) {
                const check_label = try self.generator.generateLabel("match_check");
                try check_labels.append(check_label);
            }
        }

        const case_count = match_expr.cases.len;
        for (match_expr.cases, 0..) |case, i| {
            // Add check label for cases after the first
            if (i > 0) {
                try self.generator.instructions.append(.{ .Label = .{ .name = check_labels.items[i - 1] } });
            }
            const false_label = caseFalseLabel(i, case_count, check_labels.items, fail_label, end_label);

            // A group subject decides the arm entirely: member check first,
            // then the payload's variant index when the arm names one.
            if (match_group) |group| {
                if (try self.emitGroupCaseChecks(case, i, group, .{
                    .case_labels = case_labels.items,
                    .check_labels = check_labels.items,
                    .end_label = end_label,
                    .fail_label = fail_label,
                    .case_count = case_count,
                })) continue;
            }

            // A destructuring pattern is a type test: the arm runs when the
            // subject has the named struct.
            if (case.path_patterns.len > 0 and case.path_patterns[0].field_names.len > 0 and match_group == null) {
                const path = case.path_patterns[0];
                const resolved = case.resolved[path.pattern];

                // A union subject decides the arm here: the pattern's type is
                // compared against the member index it boxed, and a member the
                // union never carries makes the arm dead code.
                if (try self.unionPatternFor(subject_type, resolved)) |union_pattern| {
                    switch (union_pattern) {
                        .member => |m| {
                            try self.generator.instructions.append(.Dup);
                            try self.generator.instructions.append(.{ .MemberCheck = .{ .members = m.indices, .expected_tag = m.tag } });
                            try self.generator.instructions.append(.{ .JumpCond = .{
                                .label_true = case_labels.items[i],
                                .label_false = false_label,
                                .condition_type = .Tetra,
                            } });
                        },
                        .never => {
                            try self.generator.instructions.append(.{ .Jump = .{ .label = false_label } });
                            dead_cases[i] = true;
                        },
                    }
                    continue;
                }

                if (self.structTypePatternMatches(subject_type, resolved)) |is_match| {
                    if (is_match) {
                        try self.generator.instructions.append(.{ .Jump = .{ .label = case_labels.items[i] } });
                    } else {
                        // The subject is a different struct: the arm can never run.
                        try self.generator.instructions.append(.{ .Jump = .{ .label = false_label } });
                        dead_cases[i] = true;
                    }
                    continue;
                }

                const pattern_token = path.tokens[path.tokens.len - 1];
                self.generator.reporter.reportCompileError(
                    tokenLocation(pattern_token),
                    ErrorCode.TYPE_MISMATCH,
                    "Cannot match {s} against struct pattern '{s}'; narrow the subject with 'as' first",
                    .{ @tagName(std.meta.activeTag(subject_type)), pattern_token.lexeme },
                );
                return ErrorList.TypeMismatch;
            }

            // Whether any pattern of the arm can still select it. An arm every
            // pattern rules out statically is dead, and its body is skipped.
            var arm_can_match = false;
            var arm_decided = false;

            for (case.patterns, case.resolved, 0..) |pattern, resolved, pattern_idx| {
                const is_last = pattern_idx + 1 == case.patterns.len;

                // Duplicate the match value for comparison (each pattern needs its own copy)
                try self.generator.instructions.append(.Dup);

                if (isElsePattern(pattern)) {
                    // Else case - always matches, pop the duplicated value
                    try self.generator.instructions.append(.Pop);
                    try self.generator.instructions.append(.{ .Jump = .{ .label = case_labels.items[i] } });
                    arm_can_match = true;
                    arm_decided = true;
                    break;
                }

                // A builtin type pattern over a dynamic subject is a runtime
                // type test. `nothing` is both the type and its only value: over
                // a union subject the arm asks which member the box holds.
                const is_type_pattern = resolved == .token and switch (pattern.type) {
                    .INT_TYPE, .FLOAT_TYPE, .STRING_TYPE, .BYTE_TYPE, .TETRA_TYPE, .NOTHING_TYPE => true,
                    .NOTHING => subject_type == .Union,
                    else => std.mem.indexOf(u8, pattern.lexeme, "[]") != null,
                };

                if (is_type_pattern) {
                    const type_name = if (pattern.type == .NOTHING) "nothing" else pattern.lexeme;
                    try self.generator.instructions.append(.{ .TypeCheck = .{ .target_type = type_name } });
                } else if (self.structTypePatternMatches(subject_type, resolved)) |is_match| {
                    // A struct subject makes a named-type pattern a type test
                    // decided here: comparing it as an enum variant or as a
                    // literal would never select the arm.
                    try self.generator.instructions.append(.Pop);
                    if (is_match) {
                        try self.generator.instructions.append(.{ .Jump = .{ .label = case_labels.items[i] } });
                        arm_can_match = true;
                        arm_decided = true;
                        break;
                    }
                    if (is_last) {
                        try self.generator.instructions.append(.{ .Jump = .{ .label = false_label } });
                        arm_decided = true;
                        break;
                    }
                    // Not this type: try the next pattern of the same arm.
                    continue;
                } else if (try self.unionPatternFor(subject_type, resolved)) |union_pattern| {
                    // A union subject is discriminated by the member index it
                    // boxed, never by an enum variant or a literal.
                    switch (union_pattern) {
                        .member => |m| try self.generator.instructions.append(.{ .MemberCheck = .{ .members = m.indices, .expected_tag = m.tag } }),
                        .never => {
                            // This type is not in the union, so the pattern can
                            // never select the arm: drop the copy under test and
                            // leave the arm to its remaining patterns.
                            try self.generator.instructions.append(.Pop);
                            if (is_last) {
                                try self.generator.instructions.append(.{ .Jump = .{ .label = false_label } });
                                arm_decided = true;
                                break;
                            }
                            continue;
                        },
                    }
                } else if (self.patternEnum(resolved)) |enum_id| {
                    // A variant of a known enum, resolved against the enum
                    // table by id.
                    const pattern_value = try self.enumVariantValue(enum_id, pattern);
                    const pattern_value_idx = try self.generator.addConstant(pattern_value);
                    try self.generator.instructions.append(.{ .Const = .{ .value = pattern_value, .constant_id = pattern_value_idx } });
                    try self.generator.instructions.append(.{ .Compare = .{ .op = .Eq, .operand_type = HIRType{ .Enum = enum_id } } });
                } else {
                    // Regular literal pattern (int/byte/float/string/tetra/nothing)
                    const lowered = lowerMatchPatternLiteral(pattern.literal);
                    const pattern_constant_idx = try self.generator.addConstant(lowered.value);
                    try self.generator.instructions.append(.{ .Const = .{ .value = lowered.value, .constant_id = pattern_constant_idx } });
                    try self.generator.instructions.append(.{ .Compare = .{ .op = .Eq, .operand_type = lowered.operand_type } });
                }

                // Every branch reaching this point emitted a test that can
                // select the arm; the jump below decides whether it does.
                arm_can_match = true;

                if (is_last) {
                    try self.generator.instructions.append(.{ .JumpCond = .{ .label_true = case_labels.items[i], .label_false = false_label, .condition_type = .Tetra } });
                } else {
                    // Not the last pattern - if this doesn't match, continue to next pattern
                    const next_pattern_label = try self.generator.generateLabel("next_pattern");
                    try self.generator.instructions.append(.{ .JumpCond = .{ .label_true = case_labels.items[i], .label_false = next_pattern_label, .condition_type = .Tetra } });
                    try self.generator.instructions.append(.{ .Label = .{ .name = next_pattern_label } });
                }
            }

            if (arm_decided and !arm_can_match) dead_cases[i] = true;
        }

        // Generate case bodies
        for (match_expr.cases, 0..) |case, i| {
            // A statically dead arm has no label and no body: nothing reaches
            // them, and emitting them would corrupt the merge state.
            if (dead_cases[i]) continue;

            try self.generator.instructions.append(.{ .Label = .{ .name = case_labels.items[i] } });

            // Struct destructuring: extract fields from matched value
            if (case.path_patterns.len > 0 and case.path_patterns[0].field_names.len > 0) {
                try self.bindDestructuredFields(case, match_group != null or subject_type == .Union);
            }

            // Drop the matched subject so the arm body starts with a clean stack.
            // Statement matches pass preserve_result=false (same as if) so arm
            // values are not left for LLVM to merge into a dead phi.
            try self.generator.instructions.append(.Pop);

            // The check that selected this arm established which member the box
            // holds, so the body reads the subject as that member: tracked as
            // the member for inference, pushed as the view so the backend
            // unwraps the box on load. Both are torn down with the arm.
            const arm_view = try self.armNarrowingView(match_expr.value, case);
            var arm_saved_type: ?HIRType = null;
            var arm_saved_narrowing: ?HIRType = null;
            if (arm_view) |view| {
                const subject_name = match_expr.value.data.Variable.lexeme;
                arm_saved_type = self.generator.getTrackedVariableType(subject_name);
                arm_saved_narrowing = self.generator.symbol_table.getVariableNarrowing(subject_name);
                try self.generator.trackVariableType(subject_name, TypeSystem.memberView(view));
                try self.generator.symbol_table.trackVariableNarrowing(subject_name, TypeSystem.memberView(view));
                try self.generator.instructions.append(.{ .NarrowVar = .{ .slot = try self.generator.slotOf(&match_expr.value.base), .var_name = subject_name, .narrowed_type = view } });
            }

            try self.generator.generateExpression(case.body, preserve_result, !preserve_result);
            if (result_box) |boxed_type| try self.generator.instructions.append(.{ .Box = .{ .boxed_type = boxed_type } });

            if (arm_view != null) {
                const subject_name = match_expr.value.data.Variable.lexeme;
                try self.generator.instructions.append(.{ .RestoreVar = .{ .slot = try self.generator.slotOf(&match_expr.value.base), .var_name = subject_name } });
                try self.generator.trackVariableType(subject_name, arm_saved_type.?);
                try self.generator.symbol_table.restoreVariableNarrowing(subject_name, arm_saved_narrowing);
            }

            try self.generator.instructions.append(.{ .Jump = .{ .label = end_label } });
        }

        if (fail_label) |fl| {
            try self.generator.instructions.append(.{ .Label = .{ .name = fl } });
            try self.generator.instructions.append(.Pop);
            if (preserve_result) {
                try self.generator.instructions.append(.{ .Unreachable = .{ .location = fail_location } });
            } else {
                try self.generator.instructions.append(.{ .Jump = .{ .label = end_label } });
            }
        }

        // End label - the stack should now contain the result from whichever case was taken
        try self.generator.instructions.append(.{ .Label = .{ .name = end_label } });
    }

    /// Bind each field a destructuring arm names as a local. The payload's
    /// struct — the type the path resolved to — owns each field's slot and
    /// type, so GetField, StoreVar and the symbol table all agree. A boxed
    /// subject (a group or a union) keeps the struct in its payload.
    fn bindDestructuredFields(self: *ControlFlowHandler, case: ast.MatchCase, boxed: bool) !void {
        const path = case.path_patterns[0];
        const struct_id: StructId = switch (case.resolved[path.pattern]) {
            .type => |ref| self.generator.semantic.struct_table.idOf(ref).?,
            .token, .variant => unreachable, // analysis reports a non-struct destructure
        };
        const fields = self.generator.semantic.struct_table.fields(struct_id) orelse &.{};

        // Dup the match value so we still have it after field extraction
        try self.generator.instructions.append(.Dup);
        if (boxed) try self.generator.instructions.append(.{ .UnboxPayload = .{} });
        // The value is now a struct_instance — dup it so GetField doesn't consume it
        try self.generator.instructions.append(.Dup);
        for (path.field_names, path.field_storages) |field_token, storage| {
            const field = for (fields) |f| {
                if (std.mem.eql(u8, f.name, field_token.lexeme)) break f;
            } else unreachable; // analysis bound only declared fields
            try self.generator.instructions.append(.Dup); // keep struct on stack
            try self.generator.instructions.append(.{ .GetField = .{
                .field_name = field_token.lexeme,
                .container_type = HIRType{ .Struct = struct_id },
                .struct_id = struct_id,
                .field_index = field.index,
                .field_type = field.hir_type,
                .field_for_peek = false,
                .nested_struct_id = field.nested_struct_id,
            } });
            try self.generator.storePlace(try self.generator.placeOf(storage, field_token.lexeme, false), field.hir_type, .rehome);
            // The body reads this binding by name; without a tracked type
            // every use of it would infer Unknown.
            try self.generator.trackVariableType(field_token.lexeme, field.hir_type);
        }
        // Pop the duplicated struct and the original value
        try self.generator.instructions.append(.Pop);
        try self.generator.instructions.append(.Pop);
    }

    /// Generate HIR for loop expressions
    pub fn generateLoop(self: *ControlFlowHandler, loop: ast.Loop, preserve_result: bool) !void {
        _ = preserve_result; // Unused parameter
        const loop_start_label = try self.generator.generateLabel("loop_start");
        const loop_body_label = try self.generator.generateLabel("loop_body");
        const loop_step_label = try self.generator.generateLabel("loop_step");
        const loop_end_label = try self.generator.generateLabel("loop_end");
        const loop_false_label = try self.generator.generateLabel("loop_false");
        const loop_exit_label = try self.generator.generateLabel("loop_exit");
        const loop_scope_id = self.generator.nextScopeId();
        const body_scope_id = self.generator.nextScopeId();
        const has_runtime_scope = self.expressionNeedsRuntimeScope(loop.body);

        // continue should jump to step if present, otherwise to start
        const continue_target = if (loop.step != null) loop_step_label else loop_start_label;
        try self.generator.pushLoopContext(loop_end_label, continue_target, loop_scope_id, body_scope_id, has_runtime_scope);

        // The loop state survives iterations. The body arena is entered once
        // and reset on each iteration instead of being recreated on every pass.
        if (has_runtime_scope) {
            try self.generator.instructions.append(.{ .EnterScope = .{ .scope_id = loop_scope_id, .var_count = 0 } });
        }

        // Initializer (var decl or expression statement)
        if (loop.var_decl) |initializer| {
            try generateStatement(self.generator, initializer.*);
        }

        if (has_runtime_scope) {
            try self.generator.instructions.append(.{ .EnterScope = .{ .scope_id = body_scope_id, .var_count = 0 } });
        }

        // Loop start - condition check
        try self.generator.instructions.append(.{ .Label = .{ .name = loop_start_label } });

        if (loop.condition) |condition| {
            try self.generator.generateExpression(condition, true, false);
        } else {
            const true_idx = try self.generator.addConstant(HIRValue{ .tetra = TETRA_TRUE });
            try self.generator.instructions.append(.{ .Const = .{ .value = HIRValue{ .tetra = TETRA_TRUE }, .constant_id = true_idx } });
        }

        try self.generator.instructions.append(.{ .JumpCond = .{ .label_true = loop_body_label, .label_false = loop_false_label, .condition_type = .Tetra } });

        // Body - reset the reusable arena at the start of each iteration.
        try self.generator.instructions.append(.{ .Label = .{ .name = loop_body_label } });
        if (has_runtime_scope) {
            try self.generator.instructions.append(.{ .ResetScope = .{ .scope_id = body_scope_id } });
        }

        // Push symbol table scope for loop body variables
        try self.generator.symbol_table.pushScope();

        const body_boundary = self.generator.deferred_stack.items.len;
        try self.generator.loop_deferred_boundaries.append(body_boundary);

        try self.generator.generateExpression(loop.body, false, false);

        // Reset the body scope before the step or next condition check.
        if (loop.step != null) {
            try self.generator.instructions.append(.{ .Label = .{ .name = loop_step_label } });
            if (has_runtime_scope) {
                try self.generator.instructions.append(.{ .ResetScope = .{ .scope_id = body_scope_id } });
            }
            self.generator.symbol_table.popScope();

            if (loop.step) |step_expr| {
                try self.generator.generateExpression(step_expr, false, false);
            }
        } else {
            if (has_runtime_scope) {
                try self.generator.instructions.append(.{ .ResetScope = .{ .scope_id = body_scope_id } });
            }
            self.generator.symbol_table.popScope();
        }

        try self.generator.instructions.append(.{ .Jump = .{ .label = loop_start_label } });
        try self.generator.instructions.append(.{ .Label = .{ .name = loop_end_label } }); // Add end label (break target)
        // Break bypasses the normal reset path, so unwind both logical scopes.
        if (has_runtime_scope) {
            try self.generator.instructions.append(.{ .ExitScope = .{ .scope_id = body_scope_id } });
            try self.generator.instructions.append(.{ .ExitScope = .{ .scope_id = loop_scope_id } });
        }
        try self.generator.instructions.append(.{ .Jump = .{ .label = loop_exit_label } });
        // Condition-false target unwinds the scopes created before the check.
        try self.generator.instructions.append(.{ .Label = .{ .name = loop_false_label } });
        if (has_runtime_scope) {
            try self.generator.instructions.append(.{ .ExitScope = .{ .scope_id = body_scope_id } });
            try self.generator.instructions.append(.{ .ExitScope = .{ .scope_id = loop_scope_id } });
        }
        try self.generator.instructions.append(.{ .Jump = .{ .label = loop_exit_label } });
        try self.generator.instructions.append(.{ .Label = .{ .name = loop_exit_label } });
        self.generator.popLoopContext();
        _ = self.generator.loop_deferred_boundaries.pop();
    }

    /// Generate HIR for block expressions
    pub fn generateBlock(self: *ControlFlowHandler, block: ast.Expr.Data, preserve_result: bool) !void {
        const block_data = block.Block;

        try self.generator.pushDeferredBlock();

        // Generate all block statements without creating scopes for simple blocks
        for (block_data.statements) |stmt| {
            try generateStatement(self.generator, stmt);
        }

        // Generate final value if present
        if (block_data.value) |value_expr| {
            if (preserve_result) {
                try self.generator.generateExpression(value_expr, true, false);
            } else {
                try self.generator.generateExpression(value_expr, false, false);
            }
        } else if (preserve_result) {
            const nothing_idx = try self.generator.addConstant(HIRValue.nothing);
            try self.generator.instructions.append(.{ .Const = .{ .value = HIRValue.nothing, .constant_id = nothing_idx } });
        }

        try self.generator.popAndEmitDeferred();
    }

    /// Generate HIR for return expressions
    pub fn generateReturn(self: *ControlFlowHandler, return_expr: ast.Expr.Data) !void {
        const return_data = return_expr.ReturnExpr;

        // TAIL CALL OPTIMIZATION: Check if return value is a direct function call
        if (return_data.value) |value| {
            if (self.generator.deferred_stack.items.len == 0) {
                if (self.generator.tryGenerateTailCall(value)) {
                    return; // Tail call replaces both Call and Return
                }
            }
            // Regular return with value
            try self.generator.generateReturnValue(value);
        } else {
            // No value - push nothing
            const nothing_idx = try self.generator.addConstant(HIRValue.nothing);
            try self.generator.instructions.append(.{ .Const = .{ .value = HIRValue.nothing, .constant_id = nothing_idx } });
        }

        // Emit all pending deferred actions before scope exit
        try self.generator.emitAllDeferredForReturn();

        // Generate Return instruction first so the VM deep-copies the return value
        // to the caller's arena before the current scope's arena is freed.
        try self.generator.instructions.append(.{ .Return = .{
            .has_value = return_data.value != null,
            .return_type = self.generator.current_function_return_type,
            .loop_scope_count = blk: {
                var count: u32 = 0;
                for (self.generator.loop_context_stack.items) |context| {
                    if (context.has_runtime_scope) count += 1;
                }
                break :blk count;
            },
        } });

        // Exit function scope after returning
        if (self.generator.current_function_scope_id) |scope_id| {
            try self.generator.instructions.append(.{ .ExitScope = .{ .scope_id = scope_id } });
        }
    }

    /// Emit a runtime trap for unreachable expressions
    pub fn generateUnreachable(self: *ControlFlowHandler, expr: *ast.Expr) !void {
        try self.generator.instructions.append(.{ .Unreachable = .{ .location = expr.base.location() } });
    }

    /// Narrowing applied to the cast subject variable inside the then/else branches.
    /// The then branch narrows the variable to the target type; the else branch
    /// narrows it to the union remainder (full union minus target).
    const CastNarrowing = struct {
        slot: Slot,
        var_name: []const u8,
        var_index: u32,
        is_local: bool,
        saved_type: HIRType,
        saved_index_members: ?[][]const u8,
        saved_narrowing: ?HIRType,
        then_type: HIRType,
        then_members: [][]const u8,
        else_type: HIRType,
        else_members: [][]const u8,
    };

    fn applyCastNarrowing(self: *ControlFlowHandler, nw: CastNarrowing, ty: HIRType, members: [][]const u8) !void {
        // `NarrowVar` carries the union view so the backend unwraps the box
        // through the member list; the symbol table tracks the member itself,
        // because that is what the value structurally is inside the branch —
        // `u + 1` is an int operation and a call argument is the member type.
        try self.generator.trackVariableType(nw.var_name, TypeSystem.memberView(ty));
        try self.generator.symbol_table.trackVariableNarrowing(nw.var_name, TypeSystem.memberView(ty));
        try self.generator.symbol_table.trackVariableUnionMembers(nw.is_local, nw.var_index, members);
        // Tell the native backend that the variable's boxed value now denotes a
        // narrower member view, so loads inside the branch unwrap it.
        try self.generator.instructions.append(.{ .NarrowVar = .{ .slot = nw.slot, .var_name = nw.var_name, .narrowed_type = ty } });
    }

    fn restoreCastNarrowing(self: *ControlFlowHandler, nw: CastNarrowing) !void {
        try self.generator.trackVariableType(nw.var_name, nw.saved_type);
        try self.generator.symbol_table.restoreVariableNarrowing(nw.var_name, nw.saved_narrowing);
        if (nw.saved_index_members) |members| {
            try self.generator.symbol_table.trackVariableUnionMembers(nw.is_local, nw.var_index, members);
        } else {
            self.generator.symbol_table.removeVariableUnionMembers(nw.is_local, nw.var_index);
        }
        try self.generator.instructions.append(.{ .RestoreVar = .{ .slot = nw.slot, .var_name = nw.var_name } });
    }

    /// What an `as` target asks for, or null for a target no union member or
    /// group member can be (a map, an anonymous struct).
    fn castTypeTest(self: *ControlFlowHandler, target: *const ast.TypeExpr) ?TypeTest {
        return switch (target.data) {
            .Basic => |b| .{ .builtin = switch (b) {
                .Integer => "int",
                .Byte => "byte",
                .Float => "float",
                .String => "string",
                .Tetra => "tetra",
                .Nothing => "nothing",
            } },
            .Custom => |custom| .{ .named = self.generator.type_system.typeForRef(custom.ref.?) },
            else => null,
        };
    }

    /// Per-branch narrowing for an `as` cast whose subject is a group-typed
    /// variable. The then view is the same single-member union view unions use,
    /// so backend loads unwrap the box identically; the else view keeps the
    /// group type so its loads stay boxed. A store into either branch re-boxes
    /// from the variable's declared type rather than from the view, so the box
    /// keeps the group's own member index.
    fn computeGroupCastNarrowing(
        self: *ControlFlowHandler,
        named: HIRType,
        slot: Slot,
        var_name: []const u8,
        var_index: u32,
        is_local: bool,
        saved_type: HIRType,
    ) !?CastNarrowing {
        const group_id = saved_type.Group;
        const group_members = self.generator.semantic.group_table.members(group_id) orelse return null;

        var then_member: ?struct { type: HIRType, qualifier: []const u8 } = null;
        var else_members = std.array_list.Managed([]const u8).init(self.generator.allocator);
        for (group_members) |member| {
            const member_type = self.generator.type_system.typeForRef(member.ref);
            if (then_member == null and isNamedType(member_type, named)) {
                then_member = .{ .type = member_type, .qualifier = member.qualifier };
            } else {
                try else_members.append(member.qualifier);
            }
        }

        const then = then_member orelse return null;
        const member_ptr = try self.generator.allocator.create(HIRType);
        member_ptr.* = then.type;
        const then_member_ptrs = try self.generator.allocator.alloc(*const HIRType, 1);
        then_member_ptrs[0] = member_ptr;
        const then_members = try self.generator.allocator.alloc([]const u8, 1);
        then_members[0] = then.qualifier;

        return CastNarrowing{
            .slot = slot,
            .var_name = var_name,
            .var_index = var_index,
            .is_local = is_local,
            .saved_type = saved_type,
            .saved_index_members = self.generator.symbol_table.getVariableUnionMembers(is_local, var_index),
            .saved_narrowing = self.generator.symbol_table.getVariableNarrowing(var_name),
            .then_type = HIRType{ .Union = .{ .id = group_id, .members = then_member_ptrs } },
            .then_members = then_members,
            .else_type = saved_type,
            .else_members = try else_members.toOwnedSlice(),
        };
    }

    /// Compute the per-branch narrowing for an `as` cast whose subject is a plain
    /// union-typed variable. Returns null when narrowing does not apply (subject is
    /// not a tracked union variable, or the target is not a member of the union).
    fn computeCastNarrowing(self: *ControlFlowHandler, cast_data: anytype) !?CastNarrowing {
        if (cast_data.value.data != .Variable) return null;
        const var_name = cast_data.value.data.Variable.lexeme;
        const slot = try self.generator.slotOf(&cast_data.value.base);
        const var_index = self.generator.symbol_table.getVariable(var_name) orelse return null;
        const is_local = self.generator.symbol_table.isLocalVariable(var_name);
        const saved_type = self.generator.getTrackedVariableType(var_name) orelse return null;
        const type_test = self.castTypeTest(cast_data.target_type) orelse return null;
        if (saved_type == .Group) {
            return switch (type_test) {
                .named => |named| self.computeGroupCastNarrowing(named, slot, var_name, var_index, is_local, saved_type),
                .builtin => null,
            };
        }
        if (saved_type != .Union) return null;

        const member_ptrs = saved_type.Union.members;
        if (member_ptrs.len == 0) return null;

        // A group target takes every member the group flattened into; any
        // other target takes the one member it names.
        const takes_all = switch (type_test) {
            .named => |named| named == .Group,
            .builtin => false,
        };
        var then_member: ?struct { ptr: *const HIRType, name: []const u8 } = null;
        const remainder_ptrs = try self.generator.allocator.alloc(*const HIRType, member_ptrs.len);
        const remainder_names = try self.generator.allocator.alloc([]const u8, member_ptrs.len);
        var remainder_len: usize = 0;
        for (member_ptrs) |mp| {
            const name = try self.generator.hirTypeToDisplayName(mp.*);
            if ((takes_all or then_member == null) and try self.memberPasses(mp.*, type_test)) {
                if (then_member == null) {
                    const narrowed = try self.narrowedMember(mp, type_test);
                    then_member = .{ .ptr = narrowed, .name = try self.generator.hirTypeToDisplayName(narrowed.*) };
                }
            } else {
                remainder_ptrs[remainder_len] = mp;
                remainder_names[remainder_len] = name;
                remainder_len += 1;
            }
        }
        const then = then_member orelse return null;

        // Represent both narrowed views as unions (even single-member) so the
        // VM and native peek paths agree: the VM keys off the runtime value while
        // the native backend resolves the active member through the union member
        // list, which preserves concrete enum/struct names.
        const then_member_ptrs = try self.generator.allocator.alloc(*const HIRType, 1);
        then_member_ptrs[0] = then.ptr;
        const then_type = HIRType{ .Union = .{ .id = saved_type.Union.id, .members = then_member_ptrs } };
        const then_members = try self.generator.allocator.alloc([]const u8, 1);
        then_members[0] = then.name;

        const else_type = HIRType{ .Union = .{ .id = saved_type.Union.id, .members = remainder_ptrs[0..remainder_len] } };

        return CastNarrowing{
            .slot = slot,
            .var_name = var_name,
            .var_index = var_index,
            .is_local = is_local,
            .saved_type = saved_type,
            .saved_index_members = self.generator.symbol_table.getVariableUnionMembers(is_local, var_index),
            .saved_narrowing = self.generator.symbol_table.getVariableNarrowing(var_name),
            .then_type = then_type,
            .then_members = then_members,
            .else_type = else_type,
            .else_members = remainder_names[0..remainder_len],
        };
    }

    /// Generate HIR for cast expressions
    pub fn generateCast(self: *ControlFlowHandler, cast_expr: *ast.Expr, preserve_result: bool) !void {
        const cast_data = cast_expr.data.Cast;
        // Every branch's value becomes the cast's: the subject read as the
        // member it was proved to hold, or a branch's own value.
        const cast_type = try self.generator.typeOf(cast_expr);
        const narrowing = try self.computeCastNarrowing(cast_data);

        // Generate the value to cast
        try self.generator.generateExpression(cast_data.value, true, false);

        // Duplicate it so we can keep original value on success path
        try self.generator.instructions.append(.Dup);

        // Map target type to a runtime name string compatible with VM getTypeString
        const target_name: []const u8 = switch (cast_data.target_type.data) {
            .Basic => |basic| switch (basic) {
                .Integer => "int",
                .Byte => "byte",
                .Float => "float",
                .String => "string",
                .Tetra => "tetra",
                .Nothing => "nothing",
            },
            // The runtime type checker distinguishes only broad categories for
            // named types, so `as Employee` asks for a struct.
            .Custom => |custom| switch (self.generator.type_system.typeForRef(custom.ref.?)) {
                .Struct => "struct",
                .Enum => "enum",
                else => "group",
            },
            .Array => |arr_type| switch (arr_type.element_type.data) {
                // Map array element types to the strings produced by VM.getTypeString
                .Basic => |elem_basic| switch (elem_basic) {
                    .Integer => "int[]",
                    .Byte => "byte[]",
                    .Float => "float[]",
                    .String => "string[]",
                    .Tetra => "tetra[]",
                    .Nothing => "array[]", // VM uses array[] for unknown/nothing
                },
                // Arrays of named types carry the type's declared name at
                // runtime (`CalcToken[]`, not `struct[]`).
                .Custom => |custom| try std.fmt.allocPrint(self.generator.allocator, "{s}[]", .{custom.ref.?.name}),
                .Struct => "struct[]",
                // Other element kinds (enum, union, map, function, auto) default to array[]
                else => "array[]",
            },
            .Map => "map",
            .Struct => "struct",
            .Enum => "enum",
            .Union => "union",
        };

        // Check runtime type against target type using dedicated TypeCheck instruction.
        // A boxed subject is discriminated by the member it holds instead: two
        // members of the same runtime category (`enum`, `struct`) would
        // otherwise be conflated by the broad categories `target_name` maps
        // to, and a group target is every member the group flattened into.
        const subject_type = try self.generator.typeOf(cast_data.value);
        const boxed_members: ?[]const u32 = switch (cast_data.target_type.data) {
            .Custom => |custom| if (subject_type == .Union or subject_type == .Group)
                try self.boxedMembersNamed(subject_type, self.generator.type_system.typeForRef(custom.ref.?))
            else
                null,
            else => null,
        };
        if (boxed_members) |members| {
            try self.generator.instructions.append(.{ .MemberCheck = .{ .members = members } });
        } else {
            try self.generator.instructions.append(.{ .TypeCheck = .{ .target_type = target_name } });
        }

        // Branch based on comparison
        const ok_label = try self.generator.generateLabel("cast_ok");
        const else_label = try self.generator.generateLabel("cast_else");
        const end_label = try self.generator.generateLabel("cast_end");
        try self.generator.instructions.append(.{ .JumpCond = .{ .label_true = ok_label, .label_false = else_label, .condition_type = .Tetra } });

        // Else branch: drop original value and evaluate else expression
        try self.generator.instructions.append(.{ .Label = .{ .name = else_label } });
        if (cast_data.decl_else) |binding| try self.bindCastDecl(cast_data.decl_name.?, binding, subject_type);
        try self.generator.instructions.append(.Pop);
        if (cast_data.else_branch) |else_expr| {
            // Preserve result only if requested by parent
            if (narrowing) |nw| try self.applyCastNarrowing(nw, nw.else_type, nw.else_members);
            try self.generator.generateExpression(else_expr, preserve_result, false);
            if (preserve_result) try self.generator.convertValue(try self.generator.typeOf(else_expr), cast_type);
            if (narrowing) |nw| try self.restoreCastNarrowing(nw);
        } else {
            // No else branch: cast must fail -> halt program
            try self.generator.instructions.append(.Halt);
        }
        try self.generator.instructions.append(.{ .Jump = .{ .label = end_label } });

        // Success branch
        try self.generator.instructions.append(.{ .Label = .{ .name = ok_label } });
        if (cast_data.decl_then) |binding| try self.bindCastDecl(cast_data.decl_name.?, binding, subject_type);
        if (cast_data.then_branch) |then_expr| {
            if (narrowing) |nw| try self.applyCastNarrowing(nw, nw.then_type, nw.then_members);
            if (then_expr.data == .Block) {
                try self.generator.generateExpression(then_expr, true, false);
                try self.generator.instructions.append(.Pop);
                if (preserve_result) {
                    try self.generator.convertValue(subject_type, cast_type);
                } else {
                    try self.generator.instructions.append(.Pop);
                }
            } else {
                try self.generator.instructions.append(.Pop);
                try self.generator.generateExpression(then_expr, preserve_result, false);
                if (preserve_result) try self.generator.convertValue(try self.generator.typeOf(then_expr), cast_type);
            }
            if (narrowing) |nw| try self.restoreCastNarrowing(nw);
        } else if (preserve_result) {
            try self.generator.convertValue(subject_type, cast_type);
        } else {
            try self.generator.instructions.append(.Pop);
        }

        // After the success path, explicitly jump to the common end label so that
        // the LLVM IR printer sees both branches as predecessors of the merge
        // point and can correctly PHI-merge the resulting value on the stack.
        try self.generator.instructions.append(.{ .Jump = .{ .label = end_label } });

        // End merge point
        try self.generator.instructions.append(.{ .Label = .{ .name = end_label } });
    }

    /// Bind the name a cast declares for one of its branches to the subject on
    /// top of the stack, of `subject_type`, read as the branch's narrowed type.
    fn bindCastDecl(self: *ControlFlowHandler, name: []const u8, binding: ast.CastBinding, subject_type: HIRType) !void {
        const narrowed = self.generator.type_system.convertTypeInfo(binding.type_info.*);
        try self.generator.instructions.append(.Dup);
        try self.generator.convertValue(subject_type, narrowed);
        try self.generator.storePlace(try self.generator.placeOf(binding.storage, name, false), narrowed, .rehome);
    }

    /// The value of the variant of enum `enum_id` that `pattern` names.
    fn enumVariantValue(self: *ControlFlowHandler, enum_id: EnumId, pattern: ast.Token) ErrorList!HIRValue {
        const table = self.generator.semantic.enum_table;
        const variants = table.variants(enum_id) orelse &.{};
        for (variants) |variant| {
            if (!std.mem.eql(u8, variant.name, pattern.lexeme)) continue;
            return .{ .enum_variant = .{
                .type_name = table.keyOf(enum_id).?,
                .variant_name = pattern.lexeme,
                .variant_index = variant.index,
                .path = null,
            } };
        }
        self.generator.reporter.reportCompileError(
            tokenLocation(pattern),
            ErrorCode.VARIABLE_NOT_FOUND,
            "Unknown enum variant '{s}' for enum '{s}'",
            .{ pattern.lexeme, table.displayName(enum_id).? },
        );
        return ErrorList.InvalidEnumVariant;
    }
};
