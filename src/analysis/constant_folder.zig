const std = @import("std");
const ast = @import("../ast/ast.zig");
const token = @import("../types/token.zig");
const TokenLiteral = @import("../types/types.zig").TokenLiteral;

const Memory = @import("../utils/memory.zig");
const Scope = Memory.Scope;

pub const ConstantFolder = struct {
    allocator: std.mem.Allocator,
    optimizations_made: u32 = 0,
    root_scope: *Scope,
    current_scope: *Scope,
    scope_child_index: std.AutoHashMap(u32, usize),
    /// Bindings whose initializers folded to literals in this pass. Semantic
    /// analysis stores placeholder values (e.g. `0`) for runtime calls, so the
    /// folder must not read those as compile-time constants.
    comptime_bindings: std.array_list.Managed(std.StringHashMap(TokenLiteral)),
    /// Names that are *not* compile-time constants in each scope: function
    /// parameters, mutable variables, non-literal constants, and loop / query
    /// bindings. These hide same-named constants from outer scopes so a
    /// reference is never folded to a value it does not actually hold.
    shadowed_names: std.array_list.Managed(std.StringHashMap(void)),

    pub fn init(allocator: std.mem.Allocator, root_scope: *Scope) ConstantFolder {
        var folder = ConstantFolder{
            .allocator = allocator,
            .root_scope = root_scope,
            .current_scope = root_scope,
            .scope_child_index = std.AutoHashMap(u32, usize).init(allocator),
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
        self.scope_child_index.deinit();
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
        var idx = self.scope_child_index.get(self.current_scope.id) orelse 0;
        const children = self.current_scope.children.items;
        while (idx < children.len) : (idx += 1) {
            const child = children[idx];
            if (!child.is_deinited) {
                self.scope_child_index.put(self.current_scope.id, idx + 1) catch {};
                self.current_scope = child;
                return;
            }
        }
    }

    fn leaveScope(self: *ConstantFolder) void {
        self.popBindingScope();
        if (self.current_scope.parent) |parent| {
            self.current_scope = parent;
        }
    }

    pub fn foldExpr(self: *ConstantFolder, expr: *ast.Expr) std.mem.Allocator.Error!*ast.Expr {
        switch (expr.data) {
            .Literal, .Input, .EnumMember, .DefaultArgPlaceholder, .Unreachable, .Break, .This, .TypeExpr, .EnumDecl, .GroupDecl => {
                return expr;
            },
            .Variable => |var_token| {
                // Substitute the constant in place so the rewrite survives even
                // where the parent cannot be re-pointed (array elements, opaque
                // argument lists).
                if (self.lookupComptime(var_token.lexeme)) |value| {
                    self.optimizations_made += 1;
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
                    if (self.foldBinaryOp(folded_left.data.Literal, binary.operator, folded_right.data.Literal)) |result| {
                        self.optimizations_made += 1;

                        folded_left.deinit(self.allocator);
                        self.allocator.destroy(folded_left);
                        folded_right.deinit(self.allocator);
                        self.allocator.destroy(folded_right);

                        expr.data = .{ .Literal = result };
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

                    const combined_elements = try self.allocator.alloc(*ast.Expr, combined_size);

                    for (0..left_array.len) |i| {
                        combined_elements[i] = left_array[i];
                    }

                    for (0..right_array.len) |i| {
                        combined_elements[left_array.len + i] = right_array[i];
                    }

                    self.optimizations_made += 1;

                    folded_left.deinit(self.allocator);
                    self.allocator.destroy(folded_left);
                    folded_right.deinit(self.allocator);
                    self.allocator.destroy(folded_right);

                    expr.data = .{ .Array = combined_elements };
                }

                return expr;
            },
            .Unary => |*unary| {
                const folded_operand = try self.foldExpr(unary.right.?);
                unary.right = folded_operand;

                if (folded_operand.data == .Literal) {
                    if (self.foldUnaryOp(unary.operator, folded_operand.data.Literal)) |result| {
                        self.optimizations_made += 1;

                        folded_operand.deinit(self.allocator);
                        self.allocator.destroy(folded_operand);

                        expr.data = .{ .Literal = result };
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

                    if (folded_condition.data == .Literal) {
                        const is_truthy = self.isTruthy(folded_condition.data.Literal);
                        if (is_truthy) {
                            if (if_expr.then_branch) |then_branch| {
                                self.optimizations_made += 1;
                                return try self.foldExpr(then_branch);
                            }
                        } else {
                            if (if_expr.else_branch) |else_branch| {
                                self.optimizations_made += 1;
                                return try self.foldExpr(else_branch);
                            } else {
                                self.optimizations_made += 1;
                                const nothing_expr = try self.allocator.create(ast.Expr);
                                nothing_expr.* = .{
                                    .base = expr.base,
                                    .data = .{ .Literal = .{ .nothing = {} } },
                                };
                                return nothing_expr;
                            }
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
            .Struct => |fields| {
                for (fields) |field| {
                    field.value = try self.foldExpr(field.value);
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
                logical.left = try self.foldExpr(logical.left);
                logical.right = try self.foldExpr(logical.right);
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
            .BuiltinCall => |*call| {
                for (call.arguments) |argument| {
                    _ = try self.foldExpr(argument);
                }
                return expr;
            },
            .Map => |*map_expr| {
                for (map_expr.entries) |entry| {
                    entry.key = try self.foldExpr(entry.key);
                    entry.value = try self.foldExpr(entry.value);
                }
                return expr;
            },
            .MapLiteral => |*map_literal| {
                for (map_literal.entries) |entry| {
                    entry.key = try self.foldExpr(entry.key);
                    entry.value = try self.foldExpr(entry.value);
                }
                if (map_literal.else_value) |else_value| {
                    map_literal.else_value = try self.foldExpr(else_value);
                }
                return expr;
            },
            .InternalCall => |*call| {
                call.receiver = try self.foldExpr(call.receiver);
                for (call.arguments) |argument| {
                    _ = try self.foldExpr(argument);
                }
                return expr;
            },
            .Assignment => |*assignment| {
                if (assignment.value) |value| {
                    assignment.value = try self.foldExpr(value);
                }
                return expr;
            },
            .CompoundAssign => |*compound| {
                if (compound.value) |value| {
                    compound.value = try self.foldExpr(value);
                }
                return expr;
            },
            .Increment => |*increment| {
                const operand = increment.*;
                expr.data.Increment = try self.foldExpr(operand);
                return expr;
            },
            .Decrement => |*decrement| {
                const operand = decrement.*;
                expr.data.Decrement = try self.foldExpr(operand);
                return expr;
            },
            .Peek => |*peek| {
                peek.expr = try self.foldExpr(peek.expr);
                return expr;
            },
            .PeekStruct => |*peek_struct| {
                peek_struct.expr = try self.foldExpr(peek_struct.expr);
                return expr;
            },
            .Print => |*print| {
                print.expr = try self.foldExpr(print.expr);
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
            .Block => |statements| {
                self.enterScope();
                defer self.leaveScope();

                for (statements) |*inner_stmt| {
                    _ = try self.foldStmt(inner_stmt);
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
            .MapLiteral => |*map_literal| {
                for (map_literal.entries) |entry| {
                    entry.key = try self.foldExpr(entry.key);
                    entry.value = try self.foldExpr(entry.value);
                }
                if (map_literal.else_value) |else_value| {
                    map_literal.else_value = try self.foldExpr(else_value);
                }
            },
            .Module => {},
            .Import => {},
            .Path => {},
            .Continue => {},
            .Break => {},
            .Assert => |*assert_stmt| {
                assert_stmt.condition = try self.foldExpr(assert_stmt.condition);
                if (assert_stmt.message) |message| {
                    assert_stmt.message = try self.foldExpr(message);
                }
            },
            .Cast => |*cast_stmt| {
                cast_stmt.value = try self.foldExpr(cast_stmt.value);
                if (cast_stmt.else_branch) |else_branch| {
                    cast_stmt.else_branch = try self.foldExpr(else_branch);
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

    fn foldBinaryOp(self: *ConstantFolder, left: TokenLiteral, operator: token.Token, right: TokenLiteral) ?TokenLiteral {
        return switch (operator.type) {
            .PLUS => self.foldAdd(left, right),
            .MINUS => self.foldSub(left, right),
            .ASTERISK => self.foldMul(left, right),
            .SLASH => self.foldDiv(left, right),
            .MODULO => self.foldMod(left, right),
            .POWER => null,

            .LESS => self.foldLess(left, right),
            .LESS_EQUAL => self.foldLessEqual(left, right),
            .GREATER => self.foldGreater(left, right),
            .GREATER_EQUAL => self.foldGreaterEqual(left, right),
            .EQUALITY => self.foldEqual(left, right),
            .BANG_EQUAL => self.foldNotEqual(left, right),

            .AND => self.foldAnd(left, right),
            .OR => self.foldOr(left, right),
            .XOR => self.foldXor(left, right),
            .IFF => self.foldIff(left, right),
            .NAND => self.foldNand(left, right),
            .NOR => self.foldNor(left, right),
            .IMPLIES => self.foldImplies(left, right),

            else => null,
        };
    }

    fn foldUnaryOp(self: *ConstantFolder, operator: token.Token, operand: TokenLiteral) ?TokenLiteral {
        _ = self;

        return switch (operator.type) {
            .MINUS => switch (operand) {
                .int => |i| TokenLiteral{ .int = -i },
                .float => |f| TokenLiteral{ .float = -f },
                else => null,
            },
            .NOT => switch (operand) {
                .tetra => |t| TokenLiteral{ .tetra = switch (t) {
                    .true => .false,
                    .false => .true,
                    .both => .neither,
                    .neither => .both,
                } },
                else => null,
            },
            else => null,
        };
    }

    fn foldAdd(self: *ConstantFolder, left: TokenLiteral, right: TokenLiteral) ?TokenLiteral {
        return switch (left) {
            .int => |l| switch (right) {
                .int => |r| TokenLiteral{ .int = l + r },
                .float => |r| TokenLiteral{ .float = @as(f64, @floatFromInt(l)) + r },
                else => null,
            },
            .byte => |l| switch (right) {
                .byte => |r| TokenLiteral{ .byte = l + r },
                .int => |r| if (r >= 0 and r <= 255) TokenLiteral{ .byte = l + @as(u8, @intCast(r)) } else null,
                else => null,
            },
            .float => |l| switch (right) {
                .float => |r| TokenLiteral{ .float = l + r },
                .int => |r| TokenLiteral{ .float = l + @as(f64, @floatFromInt(r)) },
                else => null,
            },
            .array => |l| switch (right) {
                .array => |r| {
                    const combined_elements = self.allocator.alloc(TokenLiteral, l.len + r.len) catch return null;

                    for (0..l.len) |i| {
                        combined_elements[i] = l[i];
                    }

                    for (0..r.len) |i| {
                        combined_elements[l.len + i] = r[i];
                    }

                    return TokenLiteral{ .array = combined_elements };
                },
                else => null,
            },
            else => null,
        };
    }

    fn foldSub(self: *ConstantFolder, left: TokenLiteral, right: TokenLiteral) ?TokenLiteral {
        _ = self;
        return switch (left) {
            .int => |l| switch (right) {
                .int => |r| TokenLiteral{ .int = l - r },
                .float => |r| TokenLiteral{ .float = @as(f64, @floatFromInt(l)) - r },
                else => null,
            },
            .byte => |l| switch (right) {
                .byte => |r| if (l >= r) TokenLiteral{ .byte = l - r } else null,
                .int => |r| if (r >= 0 and r <= l) TokenLiteral{ .byte = l - @as(u8, @intCast(r)) } else null,
                else => null,
            },
            .float => |l| switch (right) {
                .float => |r| TokenLiteral{ .float = l - r },
                .int => |r| TokenLiteral{ .float = l - @as(f64, @floatFromInt(r)) },
                else => null,
            },
            else => null,
        };
    }

    fn foldMul(self: *ConstantFolder, left: TokenLiteral, right: TokenLiteral) ?TokenLiteral {
        _ = self;
        return switch (left) {
            .int => |l| switch (right) {
                .int => |r| TokenLiteral{ .int = l * r },
                .float => |r| TokenLiteral{ .float = @as(f64, @floatFromInt(l)) * r },
                else => null,
            },
            .byte => |l| switch (right) {
                .byte => |r| TokenLiteral{ .byte = l * r },
                .int => |r| if (r >= 0 and r <= 255 and l * @as(u8, @intCast(r)) <= 255) TokenLiteral{ .byte = l * @as(u8, @intCast(r)) } else null,
                else => null,
            },
            .float => |l| switch (right) {
                .float => |r| TokenLiteral{ .float = l * r },
                .int => |r| TokenLiteral{ .float = l * @as(f64, @floatFromInt(r)) },
                else => null,
            },
            else => null,
        };
    }

    fn foldDiv(self: *ConstantFolder, left: TokenLiteral, right: TokenLiteral) ?TokenLiteral {
        _ = self;
        return switch (left) {
            .int => |l| switch (right) {
                .float => |r| if (r != 0.0) TokenLiteral{ .float = @as(f64, @floatFromInt(l)) / r } else null,
                .int => |r| if (r != 0) TokenLiteral{ .float = @as(f64, @floatFromInt(l)) / @as(f64, @floatFromInt(r)) } else null,
                .byte => |r| if (r != 0) TokenLiteral{ .float = @as(f64, @floatFromInt(l)) / @as(f64, @floatFromInt(r)) } else null,
                else => null,
            },
            .byte => |l| switch (right) {
                .float => |r| if (r != 0.0) TokenLiteral{ .float = @as(f64, @floatFromInt(l)) / r } else null,
                .int => |r| if (r != 0) TokenLiteral{ .float = @as(f64, @floatFromInt(l)) / @as(f64, @floatFromInt(r)) } else null,
                .byte => |r| if (r != 0) TokenLiteral{ .float = @as(f64, @floatFromInt(l)) / @as(f64, @floatFromInt(r)) } else null,
                else => null,
            },
            .float => |l| switch (right) {
                .float => |r| if (r != 0.0) TokenLiteral{ .float = l / r } else null,
                .int => |r| if (r != 0) TokenLiteral{ .float = l / @as(f64, @floatFromInt(r)) } else null,
                .byte => |r| if (r != 0) TokenLiteral{ .float = l / @as(f64, @floatFromInt(r)) } else null,
                else => null,
            },
            else => null,
        };
    }

    fn foldMod(self: *ConstantFolder, left: TokenLiteral, right: TokenLiteral) ?TokenLiteral {
        _ = self;
        return switch (left) {
            .int => |l| switch (right) {
                .int => |r| if (r != 0) TokenLiteral{ .int = @mod(l, r) } else null,
                else => null,
            },
            .byte => |l| switch (right) {
                .byte => |r| if (r != 0) TokenLiteral{ .byte = l % r } else null,
                .int => |r| if (r > 0 and r <= 255) TokenLiteral{ .byte = l % @as(u8, @intCast(r)) } else null,
                else => null,
            },
            else => null,
        };
    }

    fn foldPow(self: *ConstantFolder, left: TokenLiteral, right: TokenLiteral) ?TokenLiteral {
        _ = self;
        return switch (left) {
            .int => |l| switch (right) {
                .int => |r| if (r >= 0) TokenLiteral{ .int = std.math.pow(i32, l, @as(i32, @intCast(r))) } else null,
                else => null,
            },
            .float => |l| switch (right) {
                .float => |r| TokenLiteral{ .float = std.math.pow(f64, l, r) },
                .int => |r| TokenLiteral{ .float = std.math.pow(f64, l, @as(f64, @floatFromInt(r))) },
                else => null,
            },
            else => null,
        };
    }

    fn foldLess(self: *ConstantFolder, left: TokenLiteral, right: TokenLiteral) ?TokenLiteral {
        _ = self;
        return switch (left) {
            .int => |l| switch (right) {
                .int => |r| TokenLiteral{ .tetra = if (l < r) .true else .false },
                .float => |r| TokenLiteral{ .tetra = if (@as(f64, @floatFromInt(l)) < r) .true else .false },
                else => null,
            },
            .byte => |l| switch (right) {
                .byte => |r| TokenLiteral{ .tetra = if (l < r) .true else .false },
                .int => |r| TokenLiteral{ .tetra = if (@as(i32, l) < r) .true else .false },
                else => null,
            },
            .float => |l| switch (right) {
                .float => |r| TokenLiteral{ .tetra = if (l < r) .true else .false },
                .int => |r| TokenLiteral{ .tetra = if (l < @as(f64, @floatFromInt(r))) .true else .false },
                else => null,
            },
            else => null,
        };
    }

    fn foldLessEqual(self: *ConstantFolder, left: TokenLiteral, right: TokenLiteral) ?TokenLiteral {
        _ = self;
        return switch (left) {
            .int => |l| switch (right) {
                .int => |r| TokenLiteral{ .tetra = if (l <= r) .true else .false },
                .float => |r| TokenLiteral{ .tetra = if (@as(f64, @floatFromInt(l)) <= r) .true else .false },
                else => null,
            },
            .byte => |l| switch (right) {
                .byte => |r| TokenLiteral{ .tetra = if (l <= r) .true else .false },
                .int => |r| TokenLiteral{ .tetra = if (@as(i32, l) <= r) .true else .false },
                else => null,
            },
            .float => |l| switch (right) {
                .float => |r| TokenLiteral{ .tetra = if (l <= r) .true else .false },
                .int => |r| TokenLiteral{ .tetra = if (l <= @as(f64, @floatFromInt(r))) .true else .false },
                else => null,
            },
            else => null,
        };
    }

    fn foldGreater(self: *ConstantFolder, left: TokenLiteral, right: TokenLiteral) ?TokenLiteral {
        _ = self;
        return switch (left) {
            .int => |l| switch (right) {
                .int => |r| TokenLiteral{ .tetra = if (l > r) .true else .false },
                .float => |r| TokenLiteral{ .tetra = if (@as(f64, @floatFromInt(l)) > r) .true else .false },
                else => null,
            },
            .byte => |l| switch (right) {
                .byte => |r| TokenLiteral{ .tetra = if (l > r) .true else .false },
                .int => |r| TokenLiteral{ .tetra = if (@as(i32, l) > r) .true else .false },
                else => null,
            },
            .float => |l| switch (right) {
                .float => |r| TokenLiteral{ .tetra = if (l > r) .true else .false },
                .int => |r| TokenLiteral{ .tetra = if (l > @as(f64, @floatFromInt(r))) .true else .false },
                else => null,
            },
            else => null,
        };
    }

    fn foldGreaterEqual(self: *ConstantFolder, left: TokenLiteral, right: TokenLiteral) ?TokenLiteral {
        _ = self;
        return switch (left) {
            .int => |l| switch (right) {
                .int => |r| TokenLiteral{ .tetra = if (l >= r) .true else .false },
                .float => |r| TokenLiteral{ .tetra = if (@as(f64, @floatFromInt(l)) >= r) .true else .false },
                else => null,
            },
            .byte => |l| switch (right) {
                .byte => |r| TokenLiteral{ .tetra = if (l >= r) .true else .false },
                .int => |r| TokenLiteral{ .tetra = if (@as(i32, l) >= r) .true else .false },
                else => null,
            },
            .float => |l| switch (right) {
                .float => |r| TokenLiteral{ .tetra = if (l >= r) .true else .false },
                .int => |r| TokenLiteral{ .tetra = if (l >= @as(f64, @floatFromInt(r))) .true else .false },
                else => null,
            },
            else => null,
        };
    }

    fn foldEqual(self: *ConstantFolder, left: TokenLiteral, right: TokenLiteral) ?TokenLiteral {
        _ = self;
        return switch (left) {
            .int => |l| switch (right) {
                .int => |r| TokenLiteral{ .tetra = if (l == r) .true else .false },
                .float => |r| TokenLiteral{ .tetra = if (@as(f64, @floatFromInt(l)) == r) .true else .false },
                else => null,
            },
            .byte => |l| switch (right) {
                .byte => |r| TokenLiteral{ .tetra = if (l == r) .true else .false },
                .int => |r| TokenLiteral{ .tetra = if (@as(i32, l) == r) .true else .false },
                else => null,
            },
            .float => |l| switch (right) {
                .float => |r| TokenLiteral{ .tetra = if (l == r) .true else .false },
                .int => |r| TokenLiteral{ .tetra = if (l == @as(f64, @floatFromInt(r))) .true else .false },
                else => null,
            },
            .tetra => |l| switch (right) {
                .tetra => |r| TokenLiteral{ .tetra = if (l == r) .true else .false },
                else => null,
            },
            else => null,
        };
    }

    fn foldNotEqual(self: *ConstantFolder, left: TokenLiteral, right: TokenLiteral) ?TokenLiteral {
        if (self.foldEqual(left, right)) |equal_result| {
            return switch (equal_result.tetra) {
                .true => TokenLiteral{ .tetra = .false },
                .false => TokenLiteral{ .tetra = .true },
                .both => TokenLiteral{ .tetra = .neither },
                .neither => TokenLiteral{ .tetra = .both },
            };
        }
        return null;
    }

    fn foldAnd(self: *ConstantFolder, left: TokenLiteral, right: TokenLiteral) ?TokenLiteral {
        _ = self;
        return switch (left) {
            .tetra => |l| switch (right) {
                .tetra => |r| TokenLiteral{ .tetra = switch (l) {
                    .true => r,
                    .false => .false,
                    .both => switch (r) {
                        .true => .both,
                        .false => .false,
                        .both => .both,
                        .neither => .false,
                    },
                    .neither => switch (r) {
                        .true => .neither,
                        .false => .false,
                        .both => .false,
                        .neither => .neither,
                    },
                } },
                else => null,
            },
            else => null,
        };
    }

    fn foldOr(self: *ConstantFolder, left: TokenLiteral, right: TokenLiteral) ?TokenLiteral {
        _ = self;
        return switch (left) {
            .tetra => |l| switch (right) {
                .tetra => |r| TokenLiteral{ .tetra = switch (l) {
                    .true => .true,
                    .false => r,
                    .both => switch (r) {
                        .true => .true,
                        .false => .both,
                        .both => .both,
                        .neither => .both,
                    },
                    .neither => switch (r) {
                        .true => .true,
                        .false => .neither,
                        .both => .both,
                        .neither => .neither,
                    },
                } },
                else => null,
            },
            else => null,
        };
    }

    fn foldXor(self: *ConstantFolder, left: TokenLiteral, right: TokenLiteral) ?TokenLiteral {
        _ = self;
        return switch (left) {
            .tetra => |l| switch (right) {
                .tetra => |r| TokenLiteral{ .tetra = switch (l) {
                    .true => switch (r) {
                        .true => .false,
                        .false => .true,
                        .both => .both,
                        .neither => .neither,
                    },
                    .false => r,
                    .both => switch (r) {
                        .true => .both,
                        .false => .both,
                        .both => .both,
                        .neither => .both,
                    },
                    .neither => switch (r) {
                        .true => .neither,
                        .false => .neither,
                        .both => .both,
                        .neither => .neither,
                    },
                } },
                else => null,
            },
            else => null,
        };
    }

    fn foldIff(self: *ConstantFolder, left: TokenLiteral, right: TokenLiteral) ?TokenLiteral {
        _ = self;
        return switch (left) {
            .tetra => |l| switch (right) {
                .tetra => |r| TokenLiteral{ .tetra = switch (l) {
                    .true => r,
                    .false => switch (r) {
                        .true => .false,
                        .false => .true,
                        .both => .both,
                        .neither => .neither,
                    },
                    .both => switch (r) {
                        .true => .both,
                        .false => .both,
                        .both => .both,
                        .neither => .both,
                    },
                    .neither => switch (r) {
                        .true => .neither,
                        .false => .neither,
                        .both => .both,
                        .neither => .neither,
                    },
                } },
                else => null,
            },
            else => null,
        };
    }

    fn foldNand(self: *ConstantFolder, left: TokenLiteral, right: TokenLiteral) ?TokenLiteral {
        if (self.foldAnd(left, right)) |and_result| {
            return switch (and_result.tetra) {
                .true => TokenLiteral{ .tetra = .false },
                .false => TokenLiteral{ .tetra = .true },
                .both => TokenLiteral{ .tetra = .neither },
                .neither => TokenLiteral{ .tetra = .both },
            };
        }
        return null;
    }

    fn foldNor(self: *ConstantFolder, left: TokenLiteral, right: TokenLiteral) ?TokenLiteral {
        if (self.foldOr(left, right)) |or_result| {
            return switch (or_result.tetra) {
                .true => TokenLiteral{ .tetra = .false },
                .false => TokenLiteral{ .tetra = .true },
                .both => TokenLiteral{ .tetra = .neither },
                .neither => TokenLiteral{ .tetra = .both },
            };
        }
        return null;
    }

    fn foldImplies(self: *ConstantFolder, left: TokenLiteral, right: TokenLiteral) ?TokenLiteral {
        _ = self;
        return switch (left) {
            .tetra => |l| switch (right) {
                .tetra => |r| TokenLiteral{ .tetra = switch (l) {
                    .true => r,
                    .false => .true,
                    .both => switch (r) {
                        .true => .both,
                        .false => .both,
                        .both => .both,
                        .neither => .both,
                    },
                    .neither => switch (r) {
                        .true => .neither,
                        .false => .true,
                        .both => .both,
                        .neither => .neither,
                    },
                } },
                else => null,
            },
            else => null,
        };
    }

    fn isTruthy(self: *ConstantFolder, literal: TokenLiteral) bool {
        _ = self;
        return switch (literal) {
            .tetra => |t| switch (t) {
                .true => true,
                .false => false,
                .both => true,
                .neither => false,
            },
            .int => |i| i != 0,
            .byte => |u| u != 0,
            .float => |f| f != 0.0,
            .string => |s| s.len > 0,
            .nothing => false,
            else => true,
        };
    }

    pub fn resetCounter(self: *ConstantFolder) void {
        self.optimizations_made = 0;
    }

    pub fn getOptimizationCount(self: *ConstantFolder) u32 {
        return self.optimizations_made;
    }
};
