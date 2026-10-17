const std = @import("std");
const ast = @import("../ast/ast.zig");
const TokenLiteral = @import("../types/types.zig").TokenLiteral;
const Tetra = @import("../types/types.zig").Tetra;
const Reporter = @import("../utils/reporting.zig").Reporter;
const consteval = @import("consteval.zig");

/// Folds constant expressions in place. What an operator computes is
/// `consteval`'s; the folder only walks the tree, tracks which names are
/// constants, and rewrites a node whose operands are literals.
pub const ConstantFolder = struct {
    allocator: std.mem.Allocator,
    reporter: *Reporter,
    /// Whether a constant expression faulted (overflow, division by zero, a
    /// conversion with no value). Each fault is reported where it happened
    /// and its node left unfolded.
    faulted: bool = false,
    /// `const` bindings whose initializers folded to literals in this pass.
    comptime_bindings: std.array_list.Managed(std.StringHashMap(TokenLiteral)),
    /// Names that are *not* compile-time constants in each scope: function
    /// parameters, mutable variables, non-literal constants, and loop / query
    /// bindings. These hide same-named constants from outer scopes so a
    /// reference is never folded to a value it does not actually hold.
    shadowed_names: std.array_list.Managed(std.StringHashMap(void)),
    /// What analysis typed each node: every rewrite keeps its node's type.
    types: ?*const std.AutoHashMap(ast.NodeId, *ast.TypeInfo),

    /// A folder is syntax-local: it sees one file's statements and folds a
    /// name only to a constant it watched that file bind.
    pub fn init(allocator: std.mem.Allocator, reporter: *Reporter, types: ?*const std.AutoHashMap(ast.NodeId, *ast.TypeInfo)) ConstantFolder {
        var folder = ConstantFolder{
            .allocator = allocator,
            .reporter = reporter,
            .types = types,
            .comptime_bindings = std.array_list.Managed(std.StringHashMap(TokenLiteral)).init(allocator),
            .shadowed_names = std.array_list.Managed(std.StringHashMap(void)).init(allocator),
        };
        folder.pushBindingScope();
        return folder;
    }

    pub fn deinit(self: *ConstantFolder) void {
        while (self.comptime_bindings.items.len > 0) {
            self.popBindingScope();
        }
        self.comptime_bindings.deinit();
        self.shadowed_names.deinit();
    }

    fn pushBindingScope(self: *ConstantFolder) void {
        self.comptime_bindings.append(std.StringHashMap(TokenLiteral).init(self.allocator)) catch {};
        self.shadowed_names.append(std.StringHashMap(void).init(self.allocator)) catch {};
    }

    fn popBindingScope(self: *ConstantFolder) void {
        if (self.comptime_bindings.items.len == 0) return;
        var map = self.comptime_bindings.pop().?;
        map.deinit();
        var shadow_map = self.shadowed_names.pop().?;
        shadow_map.deinit();
    }

    fn bindComptime(self: *ConstantFolder, name: []const u8, value: TokenLiteral) void {
        if (self.comptime_bindings.items.len == 0) return;
        const map = &self.comptime_bindings.items[self.comptime_bindings.items.len - 1];
        map.put(name, value) catch {};
    }

    fn bindShadow(self: *ConstantFolder, name: []const u8) void {
        if (self.shadowed_names.items.len == 0) return;
        const map = &self.shadowed_names.items[self.shadowed_names.items.len - 1];
        map.put(name, {}) catch {};
    }

    /// Record a declaration. Constants that folded to a literal become
    /// foldable bindings; everything else (mutable variables, non-literal
    /// constants, loop counters, parameters) only hides outer bindings.
    fn bindName(self: *ConstantFolder, name: []const u8, is_mutable: bool, folded_initializer: ?*ast.Expr) void {
        if (is_mutable) return self.bindShadow(name);
        const literal: ?TokenLiteral = if (folded_initializer) |initializer| switch (initializer.data) {
            .Literal => |lit| lit,
            else => null,
        } else null;
        if (literal) |value| self.bindComptime(name, value) else self.bindShadow(name);
    }

    fn lookupComptime(self: *ConstantFolder, name: []const u8) ?TokenLiteral {
        var i = self.comptime_bindings.items.len;
        while (i > 0) {
            i -= 1;
            // A non-constant binding shadows any outer constant of the same name.
            if (self.shadowed_names.items[i].contains(name)) return null;
            if (self.comptime_bindings.items[i].get(name)) |value| return value;
        }
        return null;
    }

    fn enterScope(self: *ConstantFolder) void {
        self.pushBindingScope();
    }

    fn leaveScope(self: *ConstantFolder) void {
        self.popBindingScope();
    }

    pub fn foldExpr(self: *ConstantFolder, expr: *ast.Expr) std.mem.Allocator.Error!*ast.Expr {
        switch (expr.data) {
            .Literal, .EnumMember, .DefaultArgPlaceholder, .Unreachable, .Break, .This, .TypeExpr, .EnumDecl, .GroupDecl, .Held => {
                return expr;
            },
            .Variable => |var_token| {
                // Substitute the constant in place so the rewrite survives even
                // where the parent cannot be re-pointed (array elements, opaque
                // argument lists).
                if (self.lookupComptime(var_token.lexeme)) |value| {
                    expr.data = .{ .Literal = value };
                }
                return expr;
            },
            .Binary => |*binary| {
                const folded_left = try self.foldExpr(binary.left.?);
                const folded_right = try self.foldExpr(binary.right.?);

                binary.left = folded_left;
                binary.right = folded_right;

                if (folded_left.data == .Literal and folded_right.data == .Literal) {
                    if (self.apply(expr, consteval.binary(binary.operator.type, folded_left.data.Literal, folded_right.data.Literal))) {
                        folded_left.deinit(self.allocator);
                        self.allocator.destroy(folded_left);
                        folded_right.deinit(self.allocator);
                        self.allocator.destroy(folded_right);
                        return expr;
                    }
                }

                if (binary.operator.type == .PLUS and
                    folded_left.data == .Array and
                    folded_right.data == .Array)
                {
                    const left_array = folded_left.data.Array;
                    const right_array = folded_right.data.Array;
                    const combined_size = left_array.len + right_array.len;

                    // The combined literal takes ownership of every element;
                    // the two emptied literals give up only their slices and
                    // their nodes (`Expr.deinit` would free the elements).
                    const combined_elements = try self.allocator.alloc(*ast.Expr, combined_size);
                    @memcpy(combined_elements[0..left_array.len], left_array);
                    @memcpy(combined_elements[left_array.len..], right_array);

                    self.allocator.free(left_array);
                    self.allocator.destroy(folded_left);
                    self.allocator.free(right_array);
                    self.allocator.destroy(folded_right);

                    expr.data = .{ .Array = combined_elements };
                }

                return expr;
            },
            .Unary => |*unary| {
                const folded_operand = try self.foldExpr(unary.right.?);
                unary.right = folded_operand;

                if (folded_operand.data == .Literal) {
                    if (self.apply(expr, consteval.unary(unary.operator.type, folded_operand.data.Literal))) {
                        folded_operand.deinit(self.allocator);
                        self.allocator.destroy(folded_operand);
                        return expr;
                    }
                }
                return expr;
            },
            .Grouping => |grouping| {
                if (grouping) |inner_expr| {
                    expr.data.Grouping = try self.foldExpr(inner_expr);
                }
                return expr;
            },
            .If => |*if_expr| {
                if (if_expr.condition) |condition| {
                    const folded_condition = try self.foldExpr(condition);
                    if_expr.condition = folded_condition;

                    if (folded_condition.data == .Literal and folded_condition.data.Literal == .tetra) {
                        if (consteval.holds(folded_condition.data.Literal.tetra)) {
                            if (if_expr.then_branch) |then_branch| {
                                return try self.foldExpr(then_branch);
                            }
                        } else {
                            // The parser supplies an `else` to every `if`.
                            return try self.foldExpr(if_expr.else_branch.?);
                        }
                    }
                }

                if (if_expr.then_branch) |then_branch| {
                    if_expr.then_branch = try self.foldExpr(then_branch);
                }
                if (if_expr.else_branch) |else_branch| {
                    if_expr.else_branch = try self.foldExpr(else_branch);
                }
                return expr;
            },
            .Block => |*block| {
                self.enterScope();
                defer self.leaveScope();

                for (block.statements) |*stmt| {
                    _ = try self.foldStmt(stmt);
                }
                if (block.value) |value| {
                    block.value = try self.foldExpr(value);
                }
                return expr;
            },
            .Array => |elements| {
                for (elements) |element| {
                    _ = try self.foldExpr(element);
                }
                return expr;
            },
            .Index => |*index| {
                index.array = try self.foldExpr(index.array);
                index.index = try self.foldExpr(index.index);
                return expr;
            },
            .IndexAssign => |*assign| {
                assign.array = try self.foldExpr(assign.array);
                assign.index = try self.foldExpr(assign.index);
                assign.value = try self.foldExpr(assign.value);
                return expr;
            },
            .FunctionCall => |*call| {
                call.callee = try self.foldExpr(call.callee);
                for (call.arguments) |*arg| {
                    if (!arg.is_alias) {
                        arg.expr = try self.foldExpr(arg.expr);
                    }
                }
                return expr;
            },
            .Logical => |*logical| {
                const folded_left = try self.foldExpr(logical.left);
                logical.left = folded_left;

                // `and` and `or` short-circuit: a left side that decides
                // the value leaves the right unevaluated, so it is never
                // folded (a fault there never happens). Exactly as lowering
                // decides it (`shortCircuit`): `and` is false unless its
                // left holds, `or` true when it does.
                if (folded_left.data == .Literal and folded_left.data.Literal == .tetra) {
                    const holds = consteval.holds(folded_left.data.Literal.tetra);
                    const decided: ?Tetra = switch (logical.operator.type) {
                        .AND => if (holds) null else .false,
                        .OR => if (holds) .true else null,
                        else => null,
                    };
                    if (decided) |value| {
                        logical.left.deinit(self.allocator);
                        self.allocator.destroy(logical.left);
                        logical.right.deinit(self.allocator);
                        self.allocator.destroy(logical.right);
                        self.rewrite(expr, .{ .tetra = value });
                        return expr;
                    }
                }

                const folded_right = try self.foldExpr(logical.right);
                logical.right = folded_right;

                if (folded_left.data == .Literal and folded_right.data == .Literal) {
                    if (self.apply(expr, consteval.binary(logical.operator.type, folded_left.data.Literal, folded_right.data.Literal))) {
                        folded_left.deinit(self.allocator);
                        self.allocator.destroy(folded_left);
                        folded_right.deinit(self.allocator);
                        self.allocator.destroy(folded_right);
                    }
                }

                return expr;
            },
            .FieldAccess => |*access| {
                access.object = try self.foldExpr(access.object);
                return expr;
            },
            .StructDecl => |*struct_decl| {
                for (struct_decl.methods) |method| {
                    self.enterScope();
                    defer self.leaveScope();

                    for (method.params) |param| {
                        self.bindShadow(param.name.lexeme);
                    }
                    for (method.params) |*param| {
                        if (param.default_value) |default_value| {
                            param.default_value = try self.foldExpr(default_value);
                        }
                    }
                    for (method.body) |*inner_stmt| {
                        _ = try self.foldStmt(inner_stmt);
                    }
                }
                return expr;
            },
            .StructLiteral => |*literal| {
                for (literal.fields) |field| {
                    field.value = try self.foldExpr(field.value);
                }
                return expr;
            },
            .FieldAssignment => |*assignment| {
                assignment.object = try self.foldExpr(assignment.object);
                assignment.value = try self.foldExpr(assignment.value);
                return expr;
            },
            .CompoundAssign => |*compound| {
                // The target is a place: only its index and object fold.
                switch (compound.target.data) {
                    .Index => |*index| {
                        index.array = try self.foldExpr(index.array);
                        index.index = try self.foldExpr(index.index);
                    },
                    .FieldAccess => |*access| access.object = try self.foldExpr(access.object),
                    else => {},
                }
                compound.operation = try self.foldExpr(compound.operation);
                return expr;
            },
            .Exists => |*exists| {
                self.bindShadow(exists.variable.lexeme);
                exists.array = try self.foldExpr(exists.array);
                exists.condition = try self.foldExpr(exists.condition);
                return expr;
            },
            .ForAll => |*forall| {
                self.bindShadow(forall.variable.lexeme);
                forall.array = try self.foldExpr(forall.array);
                forall.condition = try self.foldExpr(forall.condition);
                return expr;
            },
            .ArrayType => |*array_type| {
                if (array_type.size) |size| {
                    array_type.size = try self.foldExpr(size);
                }
                return expr;
            },
            .Match => |*match_expr| {
                match_expr.value = try self.foldExpr(match_expr.value);
                for (match_expr.cases) |*case| {
                    case.body = try self.foldExpr(case.body);
                }
                return expr;
            },
            .InternalCall => |*call| {
                call.receiver = try self.foldExpr(call.receiver);
                for (call.arguments) |argument| {
                    _ = try self.foldExpr(argument);
                }
                const receiver = call.receiver;
                if (receiver.data == .Literal and self.apply(expr, consteval.convert(call.method.type, receiver.data.Literal))) {
                    receiver.deinit(self.allocator);
                    self.allocator.destroy(receiver);
                }
                return expr;
            },
            .Assignment => |*assignment| {
                if (assignment.value) |value| {
                    assignment.value = try self.foldExpr(value);
                }
                return expr;
            },
            .Peek => |*peek| {
                peek.expr = try self.foldExpr(peek.expr);
                return expr;
            },
            .InterpolatedString => |template| {
                for (template.parts) |*part| {
                    switch (part.*) {
                        .Expression => |inner_expr| {
                            part.* = .{ .Expression = try self.foldExpr(inner_expr) };
                        },
                        else => {},
                    }
                }
                return expr;
            },
            .Assert => |*assert_expr| {
                assert_expr.condition = try self.foldExpr(assert_expr.condition);
                if (assert_expr.message) |message| {
                    assert_expr.message = try self.foldExpr(message);
                }
                return expr;
            },
            .Cast => |*cast_expr| {
                cast_expr.value = try self.foldExpr(cast_expr.value);
                if (cast_expr.else_branch) |else_branch| {
                    cast_expr.else_branch = try self.foldExpr(else_branch);
                }
                return expr;
            },
            .ReturnExpr => |*ret| {
                if (ret.value) |value| {
                    ret.value = try self.foldExpr(value);
                }
                return expr;
            },
            .Loop => |*loop| {
                // What the loop iterates belongs to the scope around it.
                if (loop.over) |over| loop.over = try self.foldExpr(over);
                self.enterScope();
                defer self.leaveScope();

                // Loop counters are assigned by the loop machinery regardless of
                // how the desugared declaration flags mutability; they must
                // shadow same-named outer constants, never fold to them.
                if (loop.var_decl) |var_decl| {
                    _ = try self.foldStmt(var_decl);
                    if (var_decl.data == .VarDecl) {
                        self.bindShadow(var_decl.data.VarDecl.name.lexeme);
                    }
                }
                if (loop.condition) |condition| {
                    loop.condition = try self.foldExpr(condition);
                }
                if (loop.step) |step| {
                    loop.step = try self.foldExpr(step);
                }
                loop.body = try self.foldExpr(loop.body);
                return expr;
            },
            .Range => |*range| {
                range.start = try self.foldExpr(range.start);
                range.end = try self.foldExpr(range.end);
                return expr;
            },
        }
    }

    pub fn foldStmt(self: *ConstantFolder, stmt: *ast.Stmt) std.mem.Allocator.Error!ast.Stmt {
        switch (stmt.data) {
            .Expression => |maybe_expr| {
                if (maybe_expr) |expr| {
                    stmt.data.Expression = try self.foldExpr(expr);
                }
            },
            .ZigDecl => {},
            .VarDecl => |*var_decl| {
                const folded = if (var_decl.initializer) |initializer| try self.foldExpr(initializer) else null;
                if (folded) |f| {
                    var_decl.initializer = f;
                }
                self.bindName(var_decl.name.lexeme, var_decl.type_info.is_mutable, folded);
            },
            .Return => |*ret| {
                if (ret.value) |value| {
                    ret.value = try self.foldExpr(value);
                }
            },
            .FunctionDecl => |*func| {
                self.enterScope();
                defer self.leaveScope();

                for (func.params) |param| {
                    self.bindShadow(param.name.lexeme);
                }
                for (func.params) |*param| {
                    if (param.default_value) |default_value| {
                        param.default_value = try self.foldExpr(default_value);
                    }
                }
                for (func.body) |*inner_stmt| {
                    _ = try self.foldStmt(inner_stmt);
                }
            },
            .EnumDecl => {},
            .GroupDecl => {},
            .Import => {},
            .Continue => {},
            .Break => {},
            .Assert => |*assert_stmt| {
                assert_stmt.condition = try self.foldExpr(assert_stmt.condition);
                if (assert_stmt.message) |message| {
                    assert_stmt.message = try self.foldExpr(message);
                }
            },
            .Defer => |defer_expr| {
                stmt.data.Defer = try self.foldExpr(defer_expr);
            },
            .Lift => |*lift| {
                lift.value = try self.foldExpr(lift.value);
            },
        }
        return stmt.*;
    }

    /// Rewrite `expr` to the literal `outcome` computed, and say whether it
    /// did. A fault is a compile error at `expr`, which is left as it is.
    /// Replace `expr` by the literal `value`. The type cache is keyed by
    /// node identity, so the literal must be what analysis typed the node
    /// it replaces.
    fn rewrite(self: *ConstantFolder, expr: *ast.Expr, value: TokenLiteral) void {
        if (std.debug.runtime_safety) {
            if (self.types) |types| if (types.get(expr.base.id)) |analyzed| {
                std.debug.assert(literalIs(value, analyzed));
            };
        }
        expr.data = .{ .Literal = value };
    }

    fn literalIs(value: TokenLiteral, t: *const ast.TypeInfo) bool {
        return switch (value) {
            .int => t.base == .Int,
            .byte => t.base == .Byte,
            .float => t.base == .Float,
            .string => t.base == .String,
            .tetra => t.base == .Tetra,
            .nothing => t.base == .Nothing,
            .array, .template => false,
        };
    }

    fn apply(self: *ConstantFolder, expr: *ast.Expr, outcome: consteval.Outcome) bool {
        switch (outcome) {
            .value => |value| {
                self.rewrite(expr, value);
                return true;
            },
            .not_constant => return false,
            .fault => |fault| {
                consteval.report(self.reporter, expr.base.location(), fault);
                self.faulted = true;
                return false;
            },
        }
    }

};
