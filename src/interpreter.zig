const std = @import("std");
const Node = @import("ast.zig").Node;
const TokenKind = @import("lexer.zig").TokenKind;
const ArrayList = std.ArrayList;

pub const Interpreter = struct {
    allocator: *std.mem.Allocator,
    scopes: ArrayList(std.StringHashMap(Variable)),

    const Value = union(enum) {
        Int: i64,
        Float: f64,
        String: []const u8,
        Nothing,
    };

    const Variable = struct {
        value: Value,
        is_mutable: bool,
    };

    pub fn init(allocator: *std.mem.Allocator) !Interpreter {
        var interpreter = Interpreter{
            .allocator = allocator,
            .scopes = ArrayList(std.StringHashMap(Variable)).init(allocator.*),
        };
        try interpreter.pushScope();
        return interpreter;
    }

    fn pushScope(self: *Interpreter) !void {
        const scope = std.StringHashMap(Variable).init(self.allocator.*);
        try self.scopes.append(scope);
    }

    fn popScope(self: *Interpreter) void {
        var scope = self.scopes.pop();
        scope.deinit();
    }

    fn getCurrentScope(self: *Interpreter) *std.StringHashMap(Variable) {
        return &self.scopes.items[self.scopes.items.len - 1];
    }

    fn findVariable(self: *const Interpreter, name: []const u8) ?*const Variable {
        var i: usize = self.scopes.items.len;
        while (i > 0) {
            i -= 1;
            if (self.scopes.items[i].getPtr(name)) |var_ptr| {
                return var_ptr;
            }
        }
        return null;
    }

    pub fn evaluate(self: *Interpreter, node: *Node) !Value {
        return self.evalNode(node);
    }

    pub fn deinit(self: *Interpreter) void {
        for (self.scopes.items) |*scope| {
            var it = scope.iterator();
            while (it.next()) |entry| {
                self.allocator.free(entry.key_ptr.*);
            }
            scope.deinit();
        }
        self.scopes.deinit();
    }

    fn evalNode(self: *Interpreter, node: *Node) !Value {
        return switch (node.*) {
            .Int => |i| Value{ .Int = i.value },
            .Float => |f| Value{ .Float = f.value },
            .String => |s| Value{ .String = s.value },
            .Binary => |binary| {
                const left = try self.evalNode(binary.left);
                const right = try self.evalNode(binary.right);

                const use_float = (left == .Float or right == .Float);

                if (use_float) {
                    const left_val = switch (left) {
                        .Int => |i| @as(f64, @floatFromInt(i)),
                        .Float => |f| f,
                        else => return error.TypeMismatch,
                    };
                    const right_val = switch (right) {
                        .Int => |i| @as(f64, @floatFromInt(i)),
                        .Float => |f| f,
                        else => return error.TypeMismatch,
                    };

                    return Value{ .Float = switch (binary.operator) {
                        .Plus => left_val + right_val,
                        .Minus => left_val - right_val,
                        .Star => left_val * right_val,
                        .Slash => left_val / right_val,
                        else => return error.InvalidOperator,
                    } };
                } else {
                    const left_val = left.Int;
                    const right_val = right.Int;

                    return switch (binary.operator) {
                        .Plus => Value{ .Int = left_val + right_val },
                        .Minus => Value{ .Int = left_val - right_val },
                        .Star => Value{ .Int = left_val * right_val },
                        .Slash => Value{ .Float = @as(f64, @floatFromInt(left_val)) / @as(f64, @floatFromInt(right_val)) },
                        else => return error.InvalidOperator,
                    };
                }
            },
            .Nothing => Value{ .Nothing = {} },
            .Declaration => |decl| {
                const value = try self.evalNode(decl.value);
                
                // Type checking
                if (decl.typ != .Auto) {
                    // Verify value matches declared type
                    switch (value) {
                        .Int => if (decl.typ != .Int) return error.TypeMismatch,
                        .Float => if (decl.typ != .Float) return error.TypeMismatch,
                        .String => if (decl.typ != .String) return error.TypeMismatch,
                        .Nothing => {}, // Allow Nothing for any type
                    }
                }
                
                const key = try self.allocator.dupe(u8, decl.name);
                try self.getCurrentScope().put(key, .{
                    .value = value,
                    .is_mutable = decl.is_mutable,
                });
                return value;
            },
            .Assignment => |assignment| {
                if (self.findVariable(assignment.name)) |var_info| {
                    if (!var_info.is_mutable) {
                        return error.ConstAssignment;
                    }
                    const value = try self.evalNode(assignment.value);
                    try self.getCurrentScope().put(assignment.name, .{
                        .value = value,
                        .is_mutable = true,
                    });
                    return value;
                } else {
                    return error.UndefinedVariable;
                }
            },
            .Variable => |v| {
                if (self.findVariable(v.name)) |var_info| {
                    return var_info.value;
                } else {
                    return error.UndefinedVariable;
                }
            },
            .Print => |print_node| {
                const value = try self.evalNode(print_node.expression);
                const stdout = std.io.getStdOut().writer();
                switch (value) {
                    .Int => |n| try stdout.print("{d}", .{n}),
                    .Float => |n| try stdout.print("{d:.1}", .{n}),
                    .String => |s| try stdout.print("{s}\n", .{s}),
                    .Nothing => try stdout.print("nothing\n", .{}),
                }
                return value;
            },
            .Block => |block| {
                try self.pushScope();
                defer self.popScope();

                var last_value: Value = Value{ .Int = 0 };
                for (block.statements) |stmt| {
                    last_value = try self.evalNode(stmt);
                }
                return last_value;
            },
        };
    }

    pub fn shouldPrintResult(self: *const Interpreter, source: []const u8) bool {
        _ = self;
        const trimmed = std.mem.trim(u8, source, " \t\r\n");
        return !std.mem.endsWith(u8, trimmed, ";");
    }

    pub fn debugPrintState(self: *Interpreter) !void {
        const stdout = std.io.getStdOut().writer();

        var it = self.getCurrentScope().iterator();
        while (it.next()) |entry| {
            try stdout.print("  {s} = ", .{entry.key_ptr.*});
            switch (entry.value_ptr.value) {
                .Int => |n| try stdout.print("{d}", .{n}),
                .Float => |n| try stdout.print("{d:.1}", .{n}),
                .String => |s| try stdout.print("{s}", .{s}),
                    .Nothing => try stdout.print("nothing\n", .{}),
            }
            try stdout.print("\n", .{});
        }
    }
};
