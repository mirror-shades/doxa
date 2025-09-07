const std = @import("std");
const ast = @import("../ast/ast.zig");
const token = @import("../types/token.zig");
const TokenType = token.TokenType;
const TokenLiteral = @import("../types/types.zig").TokenLiteral;
const Tetra = @import("../types/types.zig").Tetra;

/// Constant folding optimizer that evaluates constant expressions at compile time
pub const ConstantFolder = struct {
    allocator: std.mem.Allocator,
    optimizations_made: u32 = 0,

    pub fn init(allocator: std.mem.Allocator) ConstantFolder {
        return ConstantFolder{
            .allocator = allocator,
        };
    }

    /// Fold constants in an expression, returning a potentially optimized expression
    pub fn foldExpr(self: *ConstantFolder, expr: *ast.Expr) std.mem.Allocator.Error!*ast.Expr {
        switch (expr.data) {
            .Binary => |*binary| {
                // First, recursively fold the operands
                const folded_left = try self.foldExpr(binary.left.?);
                const folded_right = try self.foldExpr(binary.right.?);

                // Update the binary expression with folded operands
                binary.left = folded_left;
                binary.right = folded_right;

                // Try to fold this binary operation if both operands are literals
                if (folded_left.data == .Literal and folded_right.data == .Literal) {
                    if (self.foldBinaryOp(folded_left.data.Literal, binary.operator, folded_right.data.Literal)) |result| {
                        self.optimizations_made += 1;

                        // Clean up the original expressions since we're replacing them
                        folded_left.deinit(self.allocator);
                        self.allocator.destroy(folded_left);
                        folded_right.deinit(self.allocator);
                        self.allocator.destroy(folded_right);

                        // Create new literal expression with the folded result
                        const folded_expr = try self.allocator.create(ast.Expr);
                        folded_expr.* = .{
                            .base = expr.base,
                            .data = .{ .Literal = result },
                        };
                        return folded_expr;
                    }
                }

                // Try to fold array literals for addition
                if (binary.operator.type == .PLUS and
                    folded_left.data == .Array and
                    folded_right.data == .Array)
                {

                    // Array concatenation - create a new array with combined elements
                    const left_array = folded_left.data.Array;
                    const right_array = folded_right.data.Array;
                    const combined_size = left_array.len + right_array.len;

                    const combined_elements = try self.allocator.alloc(*ast.Expr, combined_size);

                    // Copy elements from left array
                    for (0..left_array.len) |i| {
                        combined_elements[i] = left_array[i];
                    }

                    // Copy elements from right array
                    for (0..right_array.len) |i| {
                        combined_elements[left_array.len + i] = right_array[i];
                    }

                    self.optimizations_made += 1;

                    // Clean up the original expressions
                    folded_left.deinit(self.allocator);
                    self.allocator.destroy(folded_left);
                    folded_right.deinit(self.allocator);
                    self.allocator.destroy(folded_right);

                    // Create new array expression with combined elements
                    const folded_expr = try self.allocator.create(ast.Expr);
                    folded_expr.* = .{
                        .base = expr.base,
                        .data = .{ .Array = combined_elements },
                    };
                    return folded_expr;
                }

                return expr;
            },
            .Unary => |*unary| {
                // Recursively fold the operand
                const folded_operand = try self.foldExpr(unary.right.?);
                unary.right = folded_operand;

                // Try to fold unary operation if operand is literal
                if (folded_operand.data == .Literal) {
                    if (self.foldUnaryOp(unary.operator, folded_operand.data.Literal)) |result| {
                        self.optimizations_made += 1;

                        // Clean up the original expression
                        folded_operand.deinit(self.allocator);
                        self.allocator.destroy(folded_operand);

                        // Create new literal expression
                        const folded_expr = try self.allocator.create(ast.Expr);
                        folded_expr.* = .{
                            .base = expr.base,
                            .data = .{ .Literal = result },
                        };
                        return folded_expr;
                    }
                }
                return expr;
            },
            .Grouping => |grouping| {
                if (grouping) |inner_expr| {
                    const folded_inner = try self.foldExpr(inner_expr);
                    expr.data.Grouping = folded_inner;
                }
                return expr;
            },
            .If => |*if_expr| {
                // Fold condition
                if (if_expr.condition) |condition| {
                    const folded_condition = try self.foldExpr(condition);
                    if_expr.condition = folded_condition;

                    // If condition is a literal, we can potentially eliminate branches
                    if (folded_condition.data == .Literal) {
                        const is_truthy = self.isTruthy(folded_condition.data.Literal);
                        if (is_truthy) {
                            // Condition is always true, return then_branch
                            if (if_expr.then_branch) |then_branch| {
                                self.optimizations_made += 1;
                                return try self.foldExpr(then_branch);
                            }
                        } else {
                            // Condition is always false, return else_branch or nothing
                            if (if_expr.else_branch) |else_branch| {
                                self.optimizations_made += 1;
                                return try self.foldExpr(else_branch);
                            } else {
                                // Return nothing literal
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

                // Fold branches
                if (if_expr.then_branch) |then_branch| {
                    if_expr.then_branch = try self.foldExpr(then_branch);
                }
                if (if_expr.else_branch) |else_branch| {
                    if_expr.else_branch = try self.foldExpr(else_branch);
                }
                return expr;
            },
            .Array => |elements| {
                // Fold all array elements
                for (elements) |element| {
                    _ = try self.foldExpr(element);
                }
                return expr;
            },
            .FunctionCall => |*call| {
                // Fold function arguments
                call.callee = try self.foldExpr(call.callee);
                for (call.arguments) |*arg| {
                    arg.expr = try self.foldExpr(arg.expr);
                }
                return expr;
            },
            .Block => |*block| {
                // Fold block statements
                for (block.statements) |*stmt| {
                    _ = try self.foldStmt(stmt);
                }
                if (block.value) |value| {
                    block.value = try self.foldExpr(value);
                }
                return expr;
            },
            else => {
                // For expressions we don't handle yet, just return as-is
                return expr;
            },
        }
    }

    /// Fold constants in a statement
    pub fn foldStmt(self: *ConstantFolder, stmt: *ast.Stmt) std.mem.Allocator.Error!ast.Stmt {
        switch (stmt.data) {
            .Expression => |maybe_expr| {
                if (maybe_expr) |expr| {
                    stmt.data.Expression = try self.foldExpr(expr);
                }
            },
            .VarDecl => |*var_decl| {
                if (var_decl.initializer) |initializer| {
                    var_decl.initializer = try self.foldExpr(initializer);
                }
            },
            .Return => |*ret| {
                if (ret.value) |value| {
                    ret.value = try self.foldExpr(value);
                }
            },
            .Block => |statements| {
                for (statements) |*inner_stmt| {
                    _ = try self.foldStmt(inner_stmt);
                }
            },
            .FunctionDecl => |*func| {
                for (func.body) |*inner_stmt| {
                    _ = try self.foldStmt(inner_stmt);
                }
            },
            .Assert => |*assert_stmt| {
                assert_stmt.condition = try self.foldExpr(assert_stmt.condition);
                if (assert_stmt.message) |message| {
                    assert_stmt.message = try self.foldExpr(message);
                }
            },
            else => {
                // Other statement types don't need folding
            },
        }
        return stmt.*;
    }

    /// Fold a binary operation between two literals
    fn foldBinaryOp(self: *ConstantFolder, left: TokenLiteral, operator: token.Token, right: TokenLiteral) ?TokenLiteral {
        return switch (operator.type) {
            // Arithmetic operations
            .PLUS => self.foldAdd(left, right),
            .MINUS => self.foldSub(left, right),
            .ASTERISK => self.foldMul(left, right),
            .SLASH => self.foldDiv(left, right),
            .MODULO => self.foldMod(left, right),
            // Disable constant folding for power operator to ensure consistent behavior during debugging
            .POWER => null, // self.foldPow(left, right),

            // Comparison operations
            .LESS => self.foldLess(left, right),
            .LESS_EQUAL => self.foldLessEqual(left, right),
            .GREATER => self.foldGreater(left, right),
            .GREATER_EQUAL => self.foldGreaterEqual(left, right),
            .EQUALITY => self.foldEqual(left, right),
            .BANG_EQUAL => self.foldNotEqual(left, right),

            // Logical operations
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

    /// Fold a unary operation on a literal
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

    // Arithmetic operations
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
                    // Array concatenation
                    const combined_elements = self.allocator.alloc(TokenLiteral, l.len + r.len) catch return null;

                    // Copy elements from left array
                    for (0..l.len) |i| {
                        combined_elements[i] = l[i];
                    }

                    // Copy elements from right array
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
        // All division promotes to float. Return null on divide-by-zero.
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

    // Comparison operations
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

    // Logical operations
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
        // NAND is NOT(AND)
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
        // NOR is NOT(OR)
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

    /// Check if a literal value is truthy
    fn isTruthy(self: *ConstantFolder, literal: TokenLiteral) bool {
        _ = self;
        return switch (literal) {
            .tetra => |t| switch (t) {
                .true => true,
                .false => false,
                .both => true, // Conservative: assume might be true
                .neither => false,
            },
            .int => |i| i != 0,
            .byte => |u| u != 0,
            .float => |f| f != 0.0,
            .string => |s| s.len > 0,
            .nothing => false,
            else => true, // Conservative: assume other types are truthy
        };
    }

    /// Reset optimization counter
    pub fn resetCounter(self: *ConstantFolder) void {
        self.optimizations_made = 0;
    }

    /// Get number of optimizations made
    pub fn getOptimizationCount(self: *ConstantFolder) u32 {
        return self.optimizations_made;
    }
};
