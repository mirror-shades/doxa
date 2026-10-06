const std = @import("std");
const ast = @import("../../../ast/ast.zig");
const types = @import("../../../types/types.zig");
const HIRGenerator = @import("../soxa_generator.zig").HIRGenerator;
const HIRValue = @import("../soxa_values.zig").HIRValue;
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
const StructsHandler = @import("structs.zig").StructsHandler;
const DoxaTag = @import("../../../runtime/doxa_rt.zig").DoxaTag;

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
    /// flattened member list rather than against an enum.
    const MatchGroup = struct {
        id: u32,
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

    /// The group `subject` belongs to, or null when the subject is not
    /// group-typed. A binding's tracked custom type wins over inference so a
    /// group annotation is authoritative even when the initializer alone would
    /// not reveal it.
    fn resolveMatchGroup(self: *ControlFlowHandler, subject: *ast.Expr, subject_type: HIRType) ?MatchGroup {
        const group_table = self.generator.type_system.group_table orelse return null;

        if (subject.data == .Variable) {
            const var_name = subject.data.Variable.lexeme;
            if (self.generator.symbol_table.getVariableCustomType(var_name)) |custom_name| {
                if (group_table.getIdByName(custom_name)) |gid| {
                    return .{ .id = gid, .name = custom_name };
                }
            }
        }

        if (subject_type == .Group) {
            const name = group_table.getName(subject_type.Group) orelse return null;
            return .{ .id = subject_type.Group, .name = name };
        }
        return null;
    }

    /// Locate `member_name` among `group`'s flattened members.
    fn groupMemberIndex(self: *ControlFlowHandler, group: MatchGroup, member_name: []const u8, location: Location) ErrorList!u32 {
        const group_table = self.generator.type_system.group_table orelse return ErrorList.TypeMismatch;
        const members = group_table.members(group.id) orelse return ErrorList.TypeMismatch;
        for (members, 0..) |member, idx| {
            if (std.mem.eql(u8, member.qualifier, member_name)) return @intCast(idx);
        }
        self.generator.reporter.reportCompileError(
            location,
            ErrorCode.TYPE_MISMATCH,
            "'{s}' is not a member of group '{s}'",
            .{ member_name, group.name },
        );
        return ErrorList.TypeMismatch;
    }

    /// Emit the member check for one arm: the subject must be boxed as a group
    /// value carrying `member_index`.
    fn emitGroupMemberCheck(self: *ControlFlowHandler, member_index: u32, body_label: []const u8, fail_label: []const u8) !void {
        try self.generator.instructions.append(.Dup);
        try self.generator.instructions.append(.{ .MemberCheck = .{ .member_index = member_index } });
        try self.generator.instructions.append(.{ .JumpCond = .{
            .label_true = body_label,
            .label_false = fail_label,
            .condition_type = .Tetra,
        } });
    }

    /// Emit the payload compare that narrows an enum member's arm to a single
    /// variant (`IOError.NotFound` matching only `NotFound`).
    fn emitGroupVariantCheck(self: *ControlFlowHandler, member_type_name: []const u8, variant: ast.Token, body_label: []const u8, fail_label: []const u8) !void {
        const variant_index = try self.resolveEnumPatternVariantIndex(member_type_name, variant);

        // The group box is stripped first so the compare sees the member's own
        // discriminant, not the wrapped %DoxaValue.
        try self.generator.instructions.append(.Dup);
        try self.generator.instructions.append(.{ .UnboxPayload = .{} });

        const pattern_value = HIRValue{
            .enum_variant = HIREnum{
                .type_name = member_type_name,
                .variant_name = variant.lexeme,
                .variant_index = variant_index,
                .path = null,
            },
        };
        const pattern_idx = try self.generator.addConstant(pattern_value);
        try self.generator.instructions.append(.{ .Const = .{ .value = pattern_value, .constant_id = pattern_idx } });
        try self.generator.instructions.append(.{ .Compare = .{ .op = .Eq, .operand_type = HIRType{ .Enum = 0 } } });
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
            const member_index = try self.groupMemberIndex(group, pattern.lexeme, tokenLocation(pattern));
            try self.emitGroupMemberCheck(member_index, body_label, fail_label);
            if (pattern_idx + 1 < case.patterns.len) {
                try self.generator.instructions.append(.{ .Label = .{ .name = fail_label } });
            }
        }
        return true;
    }

    fn emitGroupPathChecks(self: *ControlFlowHandler, path_pattern: ast.MatchCase.PathPattern, group: MatchGroup, body_label: []const u8, fail_label: []const u8) !void {
        if (path_pattern.tokens.len == 0) return;
        const split = path_pattern.split(group.name);
        const member_index = try self.groupMemberIndex(group, split.member.lexeme, tokenLocation(split.member));

        // A dotted path that is not a domain wildcard carries a variant, and the
        // variant decides the arm: the member check must fall through into it
        // rather than jump straight to the body and strand it unreachable.
        const narrows = !path_pattern.is_wildcard and split.variant != null;
        const after_member = if (narrows)
            try self.generator.generateLabel("group_variant")
        else
            body_label;
        try self.emitGroupMemberCheck(member_index, after_member, fail_label);

        const variant = split.variant orelse return;
        if (path_pattern.is_wildcard) return;
        if (narrows) {
            try self.generator.instructions.append(.{ .Label = .{ .name = after_member } });
        }
        try self.emitGroupVariantCheck(split.member.lexeme, variant, body_label, fail_label);
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
        if (self.generator.semantic_function_return_types) |type_map| {
            if (type_map.get(expr.base.id)) |info| {
                if (typeNeedsRuntimeScope(info.*)) return true;
            }
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
    pub fn generateIf(self: *ControlFlowHandler, if_expr: ast.If, preserve_result: bool, should_pop_after_use: bool) (std.mem.Allocator.Error || ErrorList)!void {
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

                    // THEN branch
                    try self.generator.instructions.append(.{ .Label = .{ .name = then_label } });
                    if (preserve_result) {
                        try self.generator.generateExpression(if_expr.then_branch.?, true, should_pop_after_use);
                    } else {
                        // Statement context: do not produce a value
                        try self.generator.generateExpression(if_expr.then_branch.?, false, should_pop_after_use);
                    }
                    try self.generator.instructions.append(.{ .Jump = .{ .label = end_label } });

                    // ELSE branch
                    try self.generator.instructions.append(.{ .Label = .{ .name = else_label } });
                    if (preserve_result) {
                        try self.generator.generateExpression(if_expr.else_branch.?, true, should_pop_after_use);
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

    /// Generate HIR for match expressions
    /// The struct a match subject is. A variable initialized from a struct
    /// literal carries `.Struct = 0` plus its declared type name, so the id is
    /// resolved through that name the same way field access does.
    fn matchSubjectStructId(self: *ControlFlowHandler, subject: *ast.Expr, subject_type: HIRType) ?StructId {
        if (subject_type != .Struct) return null;
        const fallback_name: ?[]const u8 = if (subject.data == .Variable)
            self.generator.symbol_table.getVariableCustomType(subject.data.Variable.lexeme)
        else
            null;
        var structs = StructsHandler.init(self.generator);
        const id = structs.resolveStructIdFromType(subject_type, fallback_name);
        return if (id == 0) null else id;
    }

    /// When both the match subject and an identifier pattern name struct types,
    /// the arm is decided statically: `true` when the subject is that struct,
    /// `false` when it is a different one. Null leaves the pattern to the
    /// checks below.
    fn structTypePatternMatches(self: *ControlFlowHandler, subject: *ast.Expr, subject_type: HIRType, pattern: ast.Token) ?bool {
        const subject_sid = self.matchSubjectStructId(subject, subject_type) orelse return null;
        const table = self.generator.type_system.struct_table orelse return null;
        const pattern_sid = table.getIdByName(pattern.lexeme) orelse return null;
        return subject_sid == pattern_sid;
    }

    /// How a custom-type pattern resolves against a union subject. A union
    /// boxes the active member's index in `reserved` — the same box a group
    /// carries — so the arm is one bit test against that index.
    const UnionPattern = union(enum) {
        /// The subject's union boxes this type at `index`.
        member: struct {
            index: u32,
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

    /// Resolve `pattern` against the union the subject is typed as. Null when
    /// the subject is not a union or the pattern names no registered type —
    /// both are owned by the checks below.
    fn unionPatternFor(self: *ControlFlowHandler, subject_type: HIRType, pattern: ast.Token) ?UnionPattern {
        if (subject_type != .Union) return null;

        const struct_id: ?u32 = if (self.generator.type_system.struct_table) |t|
            t.getIdByName(pattern.lexeme)
        else
            null;
        const enum_id: ?u32 = if (self.generator.type_system.enum_table) |t|
            t.getIdByName(pattern.lexeme)
        else
            null;
        const group_id: ?u32 = if (self.generator.type_system.group_table) |t|
            t.getIdByName(pattern.lexeme)
        else
            null;
        if (struct_id == null and enum_id == null and group_id == null) return null;

        for (subject_type.Union.members, 0..) |member_ptr, idx| {
            switch (member_ptr.*) {
                .Struct => |sid| {
                    if (struct_id != null and sid == struct_id.?)
                        return .{ .member = .{ .index = @intCast(idx), .struct_id = sid, .tag = @intFromEnum(DoxaTag.Struct) } };
                },
                .Enum => |eid| {
                    if (enum_id != null and eid == enum_id.?)
                        return .{ .member = .{ .index = @intCast(idx), .struct_id = null, .tag = @intFromEnum(DoxaTag.Enum) } };
                },
                .Group => |gid| {
                    if (group_id != null and gid == group_id.?)
                        return .{ .member = .{ .index = @intCast(idx), .struct_id = null } };
                },
                else => {},
            }
        }
        return .never;
    }

    /// What a match pattern says the subject is once the arm runs: the member
    /// type a type pattern (`int`, `string`, `int[]`) or a custom-type pattern
    /// (`FileError`) names. Null for a pattern that selects a value rather than
    /// a type — `else`, an enum variant — and for anything unregistered.
    fn matchPatternTypeName(pattern: ast.Token) ?[]const u8 {
        if (std.mem.indexOf(u8, pattern.lexeme, "[]") != null) return pattern.lexeme;
        return switch (pattern.type) {
            .INT_TYPE, .INT => "int",
            .FLOAT_TYPE, .FLOAT => "float",
            .STRING_TYPE, .STRING => "string",
            .BYTE_TYPE, .BYTE => "byte",
            .TETRA_TYPE, .TETRA => "tetra",
            .NOTHING_TYPE, .NOTHING => "nothing",
            .IDENTIFIER => if (std.mem.eql(u8, pattern.lexeme, "else")) null else pattern.lexeme,
            else => null,
        };
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

        const pattern_token = if (case.path_patterns.len > 0)
            case.path_patterns[0].tokens[case.path_patterns[0].tokens.len - 1]
        else
            case.patterns[0];
        const target_name = matchPatternTypeName(pattern_token) orelse return null;

        for (saved_type.Union.members) |member_ptr| {
            if (!std.mem.eql(u8, try self.generator.hirTypeToDisplayName(member_ptr.*), target_name)) continue;
            const view_members = try self.generator.allocator.alloc(*const HIRType, 1);
            view_members[0] = member_ptr;
            return HIRType{ .Union = .{ .id = saved_type.Union.id, .members = view_members } };
        }
        return null;
    }

    pub fn generateMatch(self: *ControlFlowHandler, match_expr: ast.MatchExpr, preserve_result: bool) ErrorList!void {
        // The subject's type is the analyzer's answer for the subject
        // expression, whatever its form: a local, a field, an element, an
        // `each` binding or a call result all match the same way.
        const subject_type = try self.generator.typeOf(match_expr.value);
        const subject_enum_id: ?EnumId = if (subject_type == .Enum) subject_type.Enum else null;

        // TODO(plan/type-authority.md, Phase B step 3): a union's id and member
        // order still come from two derivations, and a box is built with the
        // generator's (the declared type of the slot or field it is stored
        // in), not the analyzer's. Member indexes for a union, group or struct
        // subject therefore have to be read from the same derivation that
        // boxed the value until declarations and fields are converted too.
        const boxing_subject_type = self.generator.inferTypeFromExpression(match_expr.value);

        // Enum type named by the arm patterns, for the subjects that are not
        // themselves enum-typed but whose patterns spell out an enum.
        var match_enum_type: ?[]const u8 = null;
        if (subject_enum_id) |enum_id| {
            // Arm bodies resolve a bare `.Variant` against the subject's enum,
            // under the name the generator registered it by.
            if (self.generator.type_system.enum_table) |table| {
                if (table.getName(enum_id)) |qualified| {
                    const registered = &self.generator.type_system.custom_types;
                    if (registered.contains(qualified)) {
                        match_enum_type = qualified;
                    } else if (std.mem.lastIndexOfScalar(u8, qualified, '.')) |dot| {
                        if (registered.contains(qualified[dot + 1 ..])) match_enum_type = qualified[dot + 1 ..];
                    }
                }
            }
        }

        // Enum-variant patterns carry the type in their dotted path
        // (`E.A` -> [E, A]; `ns.E.A` -> [ns, E, A]), so the enum type is the
        // token immediately before the variant.
        if (subject_enum_id == null) {
            outer: for (match_expr.cases) |case| {
                for (case.path_patterns) |pp| {
                    if (pp.tokens.len >= 2) {
                        const candidate = pp.tokens[pp.tokens.len - 2].lexeme;
                        if (self.generator.type_system.custom_types.get(candidate)) |ct| {
                            if (ct.kind == .Enum) {
                                match_enum_type = candidate;
                                break :outer;
                            }
                        }
                    }
                }
            }
        }

        // A group subject is discriminated by its boxed member index, never by
        // an enum variant index, so resolve it once and keep it out of
        // `match_enum_type` — that name only ever enumerates an enum.
        const match_group = self.resolveMatchGroup(match_expr.value, boxing_subject_type);
        if (match_group != null) match_enum_type = null;

        // Track whether any pattern is an explicit else (wildcard) to know if falling through is possible.
        var has_else_case = false;
        for (match_expr.cases) |case| {
            for (case.patterns) |pattern| {
                const is_else_pattern = pattern.type == .ELSE or
                    (pattern.type == .IDENTIFIER and std.mem.eql(u8, pattern.lexeme, "else"));
                if (is_else_pattern) {
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

        for (match_expr.cases, 0..) |case, i| {
            // Add check label for cases after the first
            if (i > 0) {
                try self.generator.instructions.append(.{ .Label = .{ .name = check_labels.items[i - 1] } });
            }

            // Handle multiple patterns for this case
            var pattern_matched = false;
            // Whether any pattern of the arm can still select it. An arm every
            // pattern rules out statically is dead, and its body is skipped.
            var arm_can_match = false;

            // A group subject decides the arm entirely: member check first,
            // then the payload's variant index when the arm names one.
            if (match_group) |group| {
                pattern_matched = try self.emitGroupCaseChecks(case, i, group, .{
                    .case_labels = case_labels.items,
                    .check_labels = check_labels.items,
                    .end_label = end_label,
                    .fail_label = fail_label,
                    .case_count = match_expr.cases.len,
                });
                if (pattern_matched) continue;
            }

            // Handle group path patterns using MemberCheck
            if (case.path_patterns.len > 0) {
                for (case.path_patterns, 0..) |path_pattern, pp_idx| {
                    if (path_pattern.tokens.len >= 2) {
                        const group_name = path_pattern.tokens[0].lexeme;
                        // Find member index by qualifier in the GroupTable
                        if (self.generator.type_system.group_table) |gtable| {
                            if (gtable.getIdByName(group_name)) |gid| {
                                if (gtable.members(gid)) |members| {
                                    for (members, 0..) |member, member_idx| {
                                        if (std.mem.eql(u8, member.qualifier, path_pattern.tokens[1].lexeme)) {
                                            // The check consumes its operand, so duplicate
                                            // the subject for it; enum/struct path patterns
                                            // emit no check here and must not leave a copy.
                                            try self.generator.instructions.append(.Dup);
                                            try self.generator.instructions.append(.{ .MemberCheck = .{ .member_index = @intCast(member_idx) } });

                                            if (pp_idx == case.path_patterns.len - 1) {
                                                const false_label = if (i < match_expr.cases.len - 1) check_labels.items[i] else if (fail_label) |fl| fl else end_label;
                                                try self.generator.instructions.append(.{ .JumpCond = .{ .label_true = case_labels.items[i], .label_false = false_label, .condition_type = .Tetra } });
                                            } else {
                                                const next_label = try self.generator.generateLabel("next_group_pattern");
                                                try self.generator.instructions.append(.{ .JumpCond = .{ .label_true = case_labels.items[i], .label_false = next_label, .condition_type = .Tetra } });
                                                try self.generator.instructions.append(.{ .Label = .{ .name = next_label } });
                                            }
                                            pattern_matched = true;
                                            break;
                                        }
                                    }
                                }
                            }
                        }
                    }
                    if (pattern_matched) break;
                }
            }
            // The path already decided this arm; re-comparing its last token as
            // a bare pattern would emit a second, wrong check.
            if (pattern_matched) continue;

            // A destructuring pattern is a type test: the arm runs when the
            // subject has the named type. A struct subject answers that here,
            // which also keeps its leading token out of the enum-variant
            // comparison below.
            if (case.path_patterns.len > 0 and case.path_patterns[0].field_names.len > 0 and match_group == null) {
                const pattern_token = case.path_patterns[0].tokens[case.path_patterns[0].tokens.len - 1];
                const pattern_location = Location{
                    .file = pattern_token.file,
                    .file_uri = pattern_token.file_uri,
                    .range = .{
                        .start_line = pattern_token.line,
                        .start_col = pattern_token.column,
                        .end_line = pattern_token.line,
                        .end_col = pattern_token.column + pattern_token.lexeme.len,
                    },
                };
                const false_label = if (i < match_expr.cases.len - 1)
                    check_labels.items[i]
                else if (fail_label) |fl| fl
                else
                    end_label;

                // A union subject decides the arm here: the pattern's type is
                // compared against the member index it boxed, and a member the
                // union never carries makes the arm dead code.
                if (self.unionPatternFor(boxing_subject_type, pattern_token)) |union_pattern| {
                    switch (union_pattern) {
                        .member => |m| {
                            try self.generator.instructions.append(.Dup);
                            try self.generator.instructions.append(.{ .MemberCheck = .{ .member_index = m.index, .expected_tag = m.tag } });
                            try self.generator.instructions.append(.{ .JumpCond = .{
                                .label_true = case_labels.items[i],
                                .label_false = false_label,
                                .condition_type = .Tetra,
                            } });
                            pattern_matched = true;
                        },
                        .never => {
                            try self.generator.instructions.append(.{ .Jump = .{ .label = false_label } });
                            pattern_matched = true;
                            dead_cases[i] = true;
                        },
                    }
                    continue;
                }

                const subject_sid = self.matchSubjectStructId(match_expr.value, boxing_subject_type);
                const pattern_sid: ?u32 = if (self.generator.type_system.struct_table) |table|
                    table.getIdByName(pattern_token.lexeme)
                else
                    null;

                if (subject_sid != null and pattern_sid != null) {
                    if (subject_sid.? == pattern_sid.?) {
                        try self.generator.instructions.append(.{ .Jump = .{ .label = case_labels.items[i] } });
                    } else {
                        // The subject is a different struct: the arm can never run.
                        try self.generator.instructions.append(.{ .Jump = .{ .label = false_label } });
                        dead_cases[i] = true;
                    }
                    pattern_matched = true;
                } else if (pattern_sid == null) {
                    self.generator.reporter.reportCompileError(
                        pattern_location,
                        ErrorCode.UNKNOWN_TYPE,
                        "Unknown struct type '{s}'",
                        .{pattern_token.lexeme},
                    );
                    return ErrorList.UnknownCustomType;
                } else {
                    self.generator.reporter.reportCompileError(
                        pattern_location,
                        ErrorCode.TYPE_MISMATCH,
                        "Cannot match {s} against struct pattern '{s}'; narrow the subject with 'as' first",
                        .{ @tagName(std.meta.activeTag(boxing_subject_type)), pattern_token.lexeme },
                    );
                    return ErrorList.TypeMismatch;
                }
                continue;
            }

            for (case.patterns, 0..) |pattern, pattern_idx| {
                // Duplicate the match value for comparison (each pattern needs its own copy)
                try self.generator.instructions.append(.Dup);

                // Treat both token type .ELSE and identifier "else" as the else-case
                const is_else_case = pattern.type == .ELSE or
                    (pattern.type == .IDENTIFIER and std.mem.eql(u8, pattern.lexeme, "else"));

                if (is_else_case) {
                    // Else case - always matches, pop the duplicated value
                    try self.generator.instructions.append(.Pop);
                    try self.generator.instructions.append(.{ .Jump = .{ .label = case_labels.items[i] } });
                    pattern_matched = true;
                    arm_can_match = true;
                    break;
                } else {
                    // Check if this is a type pattern for union matching
                    const is_type_pattern = switch (pattern.type) {
                        .INT_TYPE, .FLOAT_TYPE, .STRING_TYPE, .BYTE_TYPE, .TETRA_TYPE, .NOTHING_TYPE => true,
                        // `nothing` is both the type and its only value. Over
                        // a union subject the arm asks which member the box
                        // holds, so it is the type test.
                        .NOTHING => subject_type == .Union,
                        else => std.mem.indexOf(u8, pattern.lexeme, "[]") != null,
                    };

                    if (is_type_pattern) {
                        // This is a type pattern - use TypeCheck instruction
                        const type_name = if (pattern.type == .NOTHING) "nothing" else pattern.lexeme;
                        try self.generator.instructions.append(.{ .TypeCheck = .{ .target_type = type_name } });
                    } else if (self.structTypePatternMatches(match_expr.value, boxing_subject_type, pattern)) |is_match| {
                        // A struct subject makes an identifier pattern a type
                        // test, decided here: comparing it as an enum variant or
                        // as a literal would never select the arm.
                        try self.generator.instructions.append(.Pop);
                        if (is_match) {
                            try self.generator.instructions.append(.{ .Jump = .{ .label = case_labels.items[i] } });
                            pattern_matched = true;
                            arm_can_match = true;
                            break;
                        }
                        if (pattern_idx == case.patterns.len - 1) {
                            const false_label = if (i < match_expr.cases.len - 1)
                                check_labels.items[i]
                            else if (fail_label) |fl| fl
                            else
                                end_label;
                            try self.generator.instructions.append(.{ .Jump = .{ .label = false_label } });
                            pattern_matched = true;
                            break;
                        }
                        // Not this type: try the next pattern of the same arm.
                        continue;
                    } else if (self.unionPatternFor(boxing_subject_type, pattern)) |union_pattern| {
                        // A union subject is discriminated by the member index
                        // it boxed, never by an enum variant or a literal.
                        switch (union_pattern) {
                            .member => |m| try self.generator.instructions.append(.{ .MemberCheck = .{ .member_index = m.index, .expected_tag = m.tag } }),
                            .never => {
                                // This type is not in the union, so the arm is
                                // unreachable: drop the copy under test and
                                // leave the arm to its remaining patterns.
                                try self.generator.instructions.append(.Pop);
                                if (pattern_idx == case.patterns.len - 1) {
                                    const dead_label = if (i < match_expr.cases.len - 1)
                                        check_labels.items[i]
                                    else if (fail_label) |fl| fl
                                    else
                                        end_label;
                                    try self.generator.instructions.append(.{ .Jump = .{ .label = dead_label } });
                                    pattern_matched = true;
                                    break;
                                }
                                continue;
                            },
                        }
                    } else if (subject_enum_id) |enum_id| {
                        // An enum-typed subject: the pattern names one of that
                        // enum's variants, resolved against the enum table by
                        // id rather than by a type name recovered from syntax.
                        const variant = try self.resolveEnumVariantById(enum_id, pattern);
                        const pattern_value = HIRValue{
                            .enum_variant = HIREnum{
                                .type_name = variant.enum_name,
                                .variant_name = pattern.lexeme,
                                .variant_index = variant.index,
                                .path = null,
                            },
                        };
                        const pattern_value_idx = try self.generator.addConstant(pattern_value);
                        try self.generator.instructions.append(.{ .Const = .{ .value = pattern_value, .constant_id = pattern_value_idx } });
                        try self.generator.instructions.append(.{ .Compare = .{ .op = .Eq, .operand_type = HIRType{ .Enum = enum_id } } });
                    } else if (match_enum_type) |enum_type_name| {
                        // Generate the pattern value (enum member with proper context)
                        const variant_index = try self.resolveEnumPatternVariantIndex(enum_type_name, pattern);

                        const pattern_value = HIRValue{
                            .enum_variant = HIREnum{
                                .type_name = enum_type_name,
                                .variant_name = pattern.lexeme,
                                .variant_index = variant_index,
                                .path = null,
                            },
                        };

                        const pattern_value_idx = try self.generator.addConstant(pattern_value);
                        try self.generator.instructions.append(.{ .Const = .{ .value = pattern_value, .constant_id = pattern_value_idx } });

                        // Compare and jump if equal (use Enum operand type)
                        try self.generator.instructions.append(.{ .Compare = .{ .op = .Eq, .operand_type = HIRType{ .Enum = 0 } } });
                    } else if (self.generator.current_enum_type) |enum_type_from_context| {
                        // Fallback: if we are in an enum context (e.g., inside var decl init), use it
                        const variant_index2 = try self.resolveEnumPatternVariantIndex(enum_type_from_context, pattern);

                        const pattern_value2 = HIRValue{
                            .enum_variant = HIREnum{
                                .type_name = enum_type_from_context,
                                .variant_name = pattern.lexeme,
                                .variant_index = variant_index2,
                                .path = null,
                            },
                        };
                        const pattern_idx2 = try self.generator.addConstant(pattern_value2);
                        try self.generator.instructions.append(.{ .Const = .{ .value = pattern_value2, .constant_id = pattern_idx2 } });
                        try self.generator.instructions.append(.{ .Compare = .{ .op = .Eq, .operand_type = HIRType{ .Enum = 0 } } });
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

                    // For the last pattern in this case, determine where to jump if no match
                    if (pattern_idx == case.patterns.len - 1) {
                        // This is the last pattern for this case
                        const false_label = if (i < match_expr.cases.len - 1)
                            check_labels.items[i] // Jump to next case check
                        else if (fail_label) |fl|
                            fl // No else case - jump to fail block
                        else
                            end_label; // Last case with else - jump to end
                        try self.generator.instructions.append(.{ .JumpCond = .{ .label_true = case_labels.items[i], .label_false = false_label, .condition_type = .Tetra } });
                    } else {
                        // Not the last pattern - if this doesn't match, continue to next pattern
                        const next_pattern_label = try self.generator.generateLabel("next_pattern");
                        try self.generator.instructions.append(.{ .JumpCond = .{ .label_true = case_labels.items[i], .label_false = next_pattern_label, .condition_type = .Tetra } });
                        try self.generator.instructions.append(.{ .Label = .{ .name = next_pattern_label } });
                    }
                }
            }

            if (pattern_matched and !arm_can_match) dead_cases[i] = true;
        }

        // Generate case bodies with enum context
        for (match_expr.cases, 0..) |case, i| {
            // A statically dead arm has no label and no body: nothing reaches
            // them, and emitting them would corrupt the merge state.
            if (dead_cases[i]) continue;

            try self.generator.instructions.append(.{ .Label = .{ .name = case_labels.items[i] } });

            // Struct destructuring: extract fields from matched value
            if (case.path_patterns.len > 0 and case.path_patterns[0].field_names.len > 0) {
                // The payload's struct definition owns each field's slot and type;
                // the pattern only says which fields to bind. Resolve it once so
                // GetField, StoreVar and the symbol table all agree.
                const pattern_token = case.path_patterns[0].tokens[case.path_patterns[0].tokens.len - 1];
                const destructure_struct_id: ?StructId = if (match_group) |group| blk: {
                    const member = case.path_patterns[0].split(group.name).member.lexeme;
                    const table = self.generator.type_system.struct_table orelse break :blk null;
                    break :blk table.getIdByName(member);
                } else if (self.unionPatternFor(boxing_subject_type, pattern_token)) |union_pattern| blk: {
                    break :blk switch (union_pattern) {
                        .member => |m| m.struct_id,
                        .never => null,
                    };
                } else self.matchSubjectStructId(match_expr.value, boxing_subject_type);

                // Dup the match value so we still have it after field extraction
                try self.generator.instructions.append(.Dup);
                // A boxed subject — a group or a union — keeps the struct in its
                // payload, so unwrap it before touching fields
                if (match_group != null or boxing_subject_type == .Union) {
                    try self.generator.instructions.append(.{ .UnboxPayload = .{} });
                }
                // The value is now a struct_instance — dup it so GetField doesn't consume it
                try self.generator.instructions.append(.Dup);
                // GetField for each named field
                for (case.path_patterns[0].field_names, 0..) |field_token, fi| {
                    var field_index: u32 = @intCast(fi);
                    var field_type: HIRType = .Unknown;
                    var nested_struct_id: ?StructId = null;
                    if (destructure_struct_id) |sid| {
                        if (self.generator.type_system.struct_table) |table| {
                            if (table.fields(sid)) |fields| {
                                for (fields) |f| {
                                    if (std.mem.eql(u8, f.name, field_token.lexeme)) {
                                        field_index = f.index;
                                        field_type = f.hir_type;
                                        nested_struct_id = f.nested_struct_id;
                                        break;
                                    }
                                }
                            }
                        }
                    }
                    try self.generator.instructions.append(.Dup); // keep struct on stack
                    try self.generator.instructions.append(.{ .GetField = .{
                        .field_name = field_token.lexeme,
                        .container_type = if (destructure_struct_id) |sid| HIRType{ .Struct = sid } else .Unknown,
                        .struct_id = destructure_struct_id orelse 0,
                        .field_index = field_index,
                        .field_type = field_type,
                        .field_for_peek = false,
                        .nested_struct_id = nested_struct_id,
                    } });
                    // Store the field value into a local variable
                    const var_idx = try self.generator.getOrCreateVariable(field_token.lexeme);
                    try self.generator.instructions.append(.{ .StoreVar = .{
                        .var_index = var_idx,
                        .var_name = field_token.lexeme,
                        .scope_kind = .Local,
                        .module_context = null,
                        .expected_type = field_type,
                    } });
                    // The body reads this binding by name; without a tracked type
                    // every use of it would infer Unknown.
                    try self.generator.trackVariableType(field_token.lexeme, field_type);
                }
                // Pop the duplicated struct and the original value
                try self.generator.instructions.append(.Pop);
                try self.generator.instructions.append(.Pop);
            }

            // Set enum context for case body generation if needed
            const old_enum_context = self.generator.current_enum_type;
            if (match_enum_type) |enum_type_name| {
                self.generator.current_enum_type = enum_type_name;
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
                try self.generator.instructions.append(.{ .NarrowVar = .{ .var_name = subject_name, .narrowed_type = view } });
            }

            try self.generator.generateExpression(case.body, preserve_result, !preserve_result);

            if (arm_view != null) {
                const subject_name = match_expr.value.data.Variable.lexeme;
                try self.generator.instructions.append(.{ .RestoreVar = .{ .var_name = subject_name } });
                try self.generator.trackVariableType(subject_name, arm_saved_type.?);
                try self.generator.symbol_table.restoreVariableNarrowing(subject_name, arm_saved_narrowing);
            }

            // Restore previous enum context
            self.generator.current_enum_type = old_enum_context;

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

        // The match statement result is now on the stack and will be handled by the PHI node logic
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
        try self.generator.instructions.append(.{ .NarrowVar = .{ .var_name = nw.var_name, .narrowed_type = ty } });
    }

    fn restoreCastNarrowing(self: *ControlFlowHandler, nw: CastNarrowing) !void {
        try self.generator.trackVariableType(nw.var_name, nw.saved_type);
        try self.generator.symbol_table.restoreVariableNarrowing(nw.var_name, nw.saved_narrowing);
        if (nw.saved_index_members) |members| {
            try self.generator.symbol_table.trackVariableUnionMembers(nw.is_local, nw.var_index, members);
        } else {
            self.generator.symbol_table.removeVariableUnionMembers(nw.is_local, nw.var_index);
        }
        try self.generator.instructions.append(.{ .RestoreVar = .{ .var_name = nw.var_name } });
    }

    /// A `.Custom` type name may be written through its module
    /// (`std.json.Node`); the type tables register the bare name, so resolve
    /// back to it before any lookup or runtime type-string comparison.
    fn resolveCustomTypeName(self: *ControlFlowHandler, lexeme: []const u8) []const u8 {
        if (self.generator.isCustomType(lexeme) != null) return lexeme;
        const dot = std.mem.lastIndexOfScalar(u8, lexeme, '.') orelse return lexeme;
        const bare = lexeme[dot + 1 ..];
        return if (self.generator.isCustomType(bare) != null) bare else lexeme;
    }

    /// Per-branch narrowing for an `as` cast whose subject is a group-typed
    /// variable. The then view is the same single-member union view unions use,
    /// so backend loads unwrap the box identically; the else view keeps the
    /// group type so its loads stay boxed. A store into either branch re-boxes
    /// from the variable's declared type rather than from the view, so the box
    /// keeps the group's own member index.
    fn computeGroupCastNarrowing(
        self: *ControlFlowHandler,
        cast_data: anytype,
        var_name: []const u8,
        var_index: u32,
        is_local: bool,
        saved_type: HIRType,
    ) !?CastNarrowing {
        const target_name: []const u8 = switch (cast_data.target_type.data) {
            .Custom => |tok| self.resolveCustomTypeName(tok.lexeme),
            else => return null,
        };

        const group_id = saved_type.Group;
        const group_table = self.generator.type_system.group_table orelse return null;
        const group_members = group_table.members(group_id) orelse return null;

        var then_member_type: ?HIRType = null;
        var else_members = std.array_list.Managed([]const u8).init(self.generator.allocator);
        for (group_members) |member| {
            if (std.mem.eql(u8, member.qualifier, target_name)) {
                then_member_type = self.generator.type_system.customTypeForName(member.qualifier);
            } else {
                try else_members.append(member.qualifier);
            }
        }

        const member_type = then_member_type orelse return null;
        const member_ptr = try self.generator.allocator.create(HIRType);
        member_ptr.* = member_type;
        const then_member_ptrs = try self.generator.allocator.alloc(*const HIRType, 1);
        then_member_ptrs[0] = member_ptr;
        const then_members = try self.generator.allocator.alloc([]const u8, 1);
        then_members[0] = target_name;

        return CastNarrowing{
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
        const var_index = self.generator.symbol_table.getVariable(var_name) orelse return null;
        const is_local = self.generator.symbol_table.isLocalVariable(var_name);
        const saved_type = self.generator.getTrackedVariableType(var_name) orelse return null;
        if (saved_type == .Group) {
            return self.computeGroupCastNarrowing(cast_data, var_name, var_index, is_local, saved_type);
        }
        if (saved_type != .Union) return null;

        const member_ptrs = saved_type.Union.members;
        if (member_ptrs.len == 0) return null;

        const target_name: []const u8 = switch (cast_data.target_type.data) {
            .Basic => |b| switch (b) {
                .Integer => "int",
                .Byte => "byte",
                .Float => "float",
                .String => "string",
                .Tetra => "tetra",
                .Nothing => "nothing",
            },
            .Custom => |tok| self.resolveCustomTypeName(tok.lexeme),
            else => return null,
        };

        const full_names = try self.generator.allocator.alloc([]const u8, member_ptrs.len);
        for (member_ptrs, 0..) |mp, i| {
            full_names[i] = try self.generator.hirTypeToDisplayName(mp.*);
        }

        var then_member_ptr: ?*const HIRType = null;
        const remainder_ptrs = try self.generator.allocator.alloc(*const HIRType, member_ptrs.len);
        const remainder_names = try self.generator.allocator.alloc([]const u8, member_ptrs.len);
        var remainder_len: usize = 0;
        for (member_ptrs, 0..) |mp, i| {
            if (then_member_ptr == null and std.mem.eql(u8, full_names[i], target_name)) {
                then_member_ptr = mp;
            } else {
                remainder_ptrs[remainder_len] = mp;
                remainder_names[remainder_len] = full_names[i];
                remainder_len += 1;
            }
        }
        if (then_member_ptr == null) return null;

        // Represent both narrowed views as unions (even single-member) so the
        // VM and native peek paths agree: the VM keys off the runtime value while
        // the native backend resolves the active member through the union member
        // list, which preserves concrete enum/struct names.
        const then_member_ptrs = try self.generator.allocator.alloc(*const HIRType, 1);
        then_member_ptrs[0] = then_member_ptr.?;
        const then_type = HIRType{ .Union = .{ .id = saved_type.Union.id, .members = then_member_ptrs } };
        const then_members = try self.generator.allocator.alloc([]const u8, 1);
        then_members[0] = target_name;

        const else_remainder_ptrs = remainder_ptrs[0..remainder_len];
        const else_members = remainder_names[0..remainder_len];
        const else_type = HIRType{ .Union = .{ .id = saved_type.Union.id, .members = else_remainder_ptrs } };

        return CastNarrowing{
            .var_name = var_name,
            .var_index = var_index,
            .is_local = is_local,
            .saved_type = saved_type,
            .saved_index_members = self.generator.symbol_table.getVariableUnionMembers(is_local, var_index),
            .saved_narrowing = self.generator.symbol_table.getVariableNarrowing(var_name),
            .then_type = then_type,
            .then_members = then_members,
            .else_type = else_type,
            .else_members = else_members,
        };
    }

    /// Index of `target_name` among the members of the group `subject` is boxed
    /// as, or null when the subject is not group-typed (or the target names
    /// something the group does not contain — reported during analysis).
    fn resolveCastGroupMember(self: *ControlFlowHandler, subject: *ast.Expr, target_name: []const u8) ?u32 {
        const group_table = self.generator.type_system.group_table orelse return null;

        var group_id: ?u32 = null;
        const subject_type = self.generator.inferTypeFromExpression(subject);
        if (subject_type == .Group) group_id = subject_type.Group;
        if (group_id == null and subject.data == .Variable) {
            if (self.generator.symbol_table.getVariableCustomType(subject.data.Variable.lexeme)) |custom_name| {
                group_id = group_table.getIdByName(custom_name);
            }
        }

        const gid = group_id orelse return null;
        const members = group_table.members(gid) orelse return null;
        for (members, 0..) |member, idx| {
            if (std.mem.eql(u8, member.qualifier, target_name)) return @intCast(idx);
        }
        return null;
    }

    /// Generate HIR for cast expressions
    pub fn generateCast(self: *ControlFlowHandler, cast_expr: ast.Expr.Data, preserve_result: bool) !void {
        const cast_data = cast_expr.Cast;
        const narrowing = try self.computeCastNarrowing(cast_data);

        // Generate the value to cast
        try self.generator.generateExpression(cast_data.value, true, false);

        // If this cast initializes a declaration, consume the target so nested
        // casts in the branches don't inherit it. The declared binding is written
        // once, after the cast resolves, by the declaration's own `StoreDecl`
        // (which narrows the subject to the declared type). An earlier attempt to
        // also store the raw subject here, so the name was readable inside the
        // then/else branches, emitted a full `DoxaValue` store into the
        // scalar-typed global slot and corrupted adjacent globals; see
        // TODO(branch-binding): reintroduce a correctly narrowed branch store.
        if (self.generator.cast_decl_var_index != null) {
            self.generator.cast_decl_var_index = null;
            self.generator.cast_decl_var_name = null;
        }

        // Duplicate it so we can keep original value on success path
        try self.generator.instructions.append(.Dup);

        // Map target type to a runtime name string compatible with VM getTypeString
        const target_name: []const u8 = blk: {
            switch (cast_data.target_type.data) {
                .Basic => |basic| switch (basic) {
                    .Integer => break :blk "int",
                    .Byte => break :blk "byte",
                    .Float => break :blk "float",
                    .String => break :blk "string",
                    .Tetra => break :blk "tetra",
                    .Nothing => break :blk "nothing",
                },
                .Custom => |tok| {
                    // The runtime type checker currently distinguishes only broad categories
                    // like "struct" and "enum". For custom types, map to the appropriate
                    // runtime category so `as Employee` works for compiled code.
                    const name = self.resolveCustomTypeName(tok.lexeme);
                    if (self.generator.isCustomType(name)) |ct| {
                        break :blk switch (ct.kind) {
                            .Struct => "struct",
                            .Enum => "enum",
                            .Group => "group",
                        };
                    }
                    break :blk name;
                },
                .Array => |arr_type| {
                    // Map array element types to the strings produced by VM.getTypeString
                    switch (arr_type.element_type.data) {
                        .Basic => |elem_basic| switch (elem_basic) {
                            .Integer => break :blk "int[]",
                            .Byte => break :blk "byte[]",
                            .Float => break :blk "float[]",
                            .String => break :blk "string[]",
                            .Tetra => break :blk "tetra[]",
                            .Nothing => break :blk "array[]", // VM uses array[] for unknown/nothing
                        },
                        // Arrays of custom/struct types use the actual type name at runtime
                        // e.g., CalcToken[] not struct[]
                        .Custom => |elem_tok| {
                            const name_with_brackets = std.fmt.allocPrint(self.generator.allocator, "{s}[]", .{elem_tok.lexeme}) catch break :blk "struct[]";
                            break :blk name_with_brackets;
                        },
                        .Struct => break :blk "struct[]",
                        // Other element kinds (enum, union, map, function, auto) default to array[]
                        else => break :blk "array[]",
                    }
                },
                .Map => break :blk "map",
                .Struct => break :blk "struct",
                .Enum => break :blk "enum",
                .Union => break :blk "union",
            }
        };

        // Check runtime type against target type using dedicated TypeCheck instruction
        // A group subject is discriminated by its boxed member index instead: two
        // members of the same runtime category (`enum`, `struct`) would otherwise
        // be conflated by the broad categories `target_name` maps to.
        const group_member_index: ?u32 = switch (cast_data.target_type.data) {
            .Custom => |tok| self.resolveCastGroupMember(cast_data.value, self.resolveCustomTypeName(tok.lexeme)),
            else => null,
        };
        if (group_member_index) |member_index| {
            try self.generator.instructions.append(.{ .MemberCheck = .{ .member_index = member_index } });
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
        try self.generator.instructions.append(.Pop);
        if (cast_data.else_branch) |else_expr| {
            // Preserve result only if requested by parent
            if (narrowing) |nw| try self.applyCastNarrowing(nw, nw.else_type, nw.else_members);
            try self.generator.generateExpression(else_expr, preserve_result, false);
            if (narrowing) |nw| try self.restoreCastNarrowing(nw);
        } else {
            // No else branch: cast must fail -> halt program
            try self.generator.instructions.append(.Halt);
        }
        try self.generator.instructions.append(.{ .Jump = .{ .label = end_label } });

        // Success branch
        try self.generator.instructions.append(.{ .Label = .{ .name = ok_label } });
        if (cast_data.then_branch) |then_expr| {
            if (narrowing) |nw| try self.applyCastNarrowing(nw, nw.then_type, nw.then_members);
            if (then_expr.data == .Block) {
                try self.generator.generateExpression(then_expr, true, false);
                try self.generator.instructions.append(.Pop);
                if (!preserve_result) {
                    try self.generator.instructions.append(.Pop);
                }
            } else {
                try self.generator.instructions.append(.Pop);
                try self.generator.generateExpression(then_expr, preserve_result, false);
            }
            if (narrowing) |nw| try self.restoreCastNarrowing(nw);
        } else {
            if (!preserve_result) {
                try self.generator.instructions.append(.Pop);
            }
        }

        // After the success path, explicitly jump to the common end label so that
        // the LLVM IR printer sees both branches as predecessors of the merge
        // point and can correctly PHI-merge the resulting value on the stack.
        try self.generator.instructions.append(.{ .Jump = .{ .label = end_label } });

        // End merge point
        try self.generator.instructions.append(.{ .Label = .{ .name = end_label } });
    }

    const ResolvedVariant = struct { enum_name: []const u8, index: u32 };

    /// The variant of enum `enum_id` that `pattern` names.
    fn resolveEnumVariantById(self: *ControlFlowHandler, enum_id: EnumId, pattern: ast.Token) ErrorList!ResolvedVariant {
        const location = Location{
            .file = pattern.file,
            .file_uri = pattern.file_uri,
            .range = .{
                .start_line = pattern.line,
                .start_col = pattern.column,
                .end_line = pattern.line,
                .end_col = pattern.column + pattern.lexeme.len,
            },
        };
        const table = self.generator.type_system.enum_table orelse return ErrorList.UnknownCustomType;
        const enum_name = table.getName(enum_id) orelse return ErrorList.UnknownCustomType;
        const variants = table.variants(enum_id) orelse return ErrorList.UnknownCustomType;
        for (variants) |variant| {
            if (std.mem.eql(u8, variant.name, pattern.lexeme)) return .{ .enum_name = enum_name, .index = variant.index };
        }
        self.generator.reporter.reportCompileError(
            location,
            ErrorCode.VARIABLE_NOT_FOUND,
            "Unknown enum variant '{s}' for enum '{s}'",
            .{ pattern.lexeme, enum_name },
        );
        return ErrorList.InvalidEnumVariant;
    }

    fn resolveEnumPatternVariantIndex(self: *ControlFlowHandler, enum_type_name: []const u8, pattern: ast.Token) ErrorList!u32 {
        const location = Location{
            .file = pattern.file,
            .file_uri = pattern.file_uri,
            .range = .{
                .start_line = pattern.line,
                .start_col = pattern.column,
                .end_line = pattern.line,
                .end_col = pattern.column + pattern.lexeme.len,
            },
        };

        if (self.generator.type_system.custom_types.get(enum_type_name)) |custom_type| {
            if (custom_type.kind != .Enum) {
                self.generator.reporter.reportCompileError(
                    location,
                    ErrorCode.TYPE_MISMATCH,
                    "'{s}' is not an enum type",
                    .{enum_type_name},
                );
                return ErrorList.TypeMismatch;
            }
            if (custom_type.getEnumVariantIndex(pattern.lexeme)) |variant_index| {
                return variant_index;
            }
            self.generator.reporter.reportCompileError(
                location,
                ErrorCode.VARIABLE_NOT_FOUND,
                "Unknown enum variant '{s}' for enum '{s}'",
                .{ pattern.lexeme, enum_type_name },
            );
            return ErrorList.InvalidEnumVariant;
        }

        self.generator.reporter.reportCompileError(
            location,
            ErrorCode.UNKNOWN_TYPE,
            "Unknown enum type '{s}'",
            .{enum_type_name},
        );
        return ErrorList.UnknownCustomType;
    }
};
