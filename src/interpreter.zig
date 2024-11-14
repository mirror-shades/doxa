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
        True,
        False,
        And,
        Or,
        Array: []Value,
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
                // Free variable names
                self.allocator.free(entry.key_ptr.*);
                // Free string values within variables
                const variable = entry.value_ptr.*;
                self.freeValue(variable.value);
            }
            scope.deinit();
        }
        self.scopes.deinit();
    }

    fn freeValue(self: *Interpreter, value: Value) void {
        switch (value) {
            .String => |s| self.allocator.free(s),
            .Array => |arr| {
                // Recursively free elements of the array
                for (arr) |element| {
                    self.freeValue(element);
                }
                // Free the array itself
                self.allocator.free(arr);
            },
            else => {},
        }
    }

    fn _and(a: Value, b: Value) !Value {
        return if (a == .True and b == .True) Value{ .True = {} } else Value{ .False = {} };
    }

    fn _or(a: Value, b: Value) !Value {
        return if (a == .True or b == .True) Value{ .True = {} } else Value{ .False = {} };
    }

    fn valuesEqual(a: Value, b: Value) bool {
        return switch (a) {
            .Int => |ai| switch (b) { .Int => |bi| ai == bi, else => false },
            .Float => |af| switch (b) { .Float => |bf| af == bf, else => false },
            .String => |as| switch (b) { .String => |bs| std.mem.eql(u8, as, bs), else => false },
            .Nothing => switch (b) { .Nothing => true, else => false },
            .True => switch (b) { .True => true, else => false },
            .False => switch (b) { .False => true, else => false },
            .And => switch (b) { .And => try _and(a, b), else => false },
            .Or => switch (b) { .Or => try _or(a, b), else => false },
            .Array => |aa| switch (b) {
                .Array => |ba| {
                    if (aa.len != ba.len) return false;
                    var i: usize = 0;
                    for (aa) |element| {
                        if (!valuesEqual(element, ba[i])) return false;
                        i += 1;
                    }
                    return true;
                },
                else => false,
            },
        };
    }

    fn evalNode(self: *Interpreter, node: *Node) !Value {
        return switch (node.*) {
            .Int => |i| Value{ .Int = i.value },
            .Float => |f| Value{ .Float = f.value },
            .String => |s| Value{ .String = s.value },
            .True => Value{ .True = {} },
            .False => Value{ .False = {} },
            .Binary => |binary| {
                const left = try self.evalNode(binary.left);
                const right = try self.evalNode(binary.right);
                const left_typ = binary.left.typ();
                const right_typ = binary.right.typ();

                const use_float = (left_typ == .Float or right_typ == .Float);

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
                        .EqualEqual => {
                            return if (left_val == right_val and left_typ == right_typ) Value{ .True = {} } else Value{ .False = {} };
                        },
                        .NotEqual => {
                            return if (left_val != right_val or left_typ != right_typ) Value{ .True = {} } else Value{ .False = {} };
                        },
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
                        .EqualEqual => if (left_val == right_val) Value{ .True = {} } else Value{ .False = {} },
                        .NotEqual => if (left_val != right_val) Value{ .True = {} } else Value{ .False = {} },
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
                        .True => if (decl.typ != .Bool) return error.TypeMismatch,
                        .False => if (decl.typ != .Bool) return error.TypeMismatch,
                        .Array => |_| if (decl.typ != .Array) return error.TypeMismatch,
                        .And => if (decl.typ != .Bool) return error.TypeMismatch,
                        .Or => if (decl.typ != .Bool) return error.TypeMismatch,
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
                try self.printValue(value);
                try std.io.getStdOut().writer().print("\n", .{});
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
            .Array => |array| {
                var evaluated_elements = ArrayList(Value).init(self.allocator.*);
                defer evaluated_elements.deinit();

                for (array.elements) |element| {
                    const value = try self.evalNode(element);
                    try evaluated_elements.append(value);
                }

                return Value{ .Array = try evaluated_elements.toOwnedSlice() };
            },
            .And => |and_node| {
                const left = try self.evalNode(and_node.left);
                const right = try self.evalNode(and_node.right);
                return try _and(left, right);
            },
            .Or => |or_node| {
                const left = try self.evalNode(or_node.left);
                const right = try self.evalNode(or_node.right);
                return try _or(left, right);
            },
        };
    }

    pub fn shouldPrintResult(self: *const Interpreter, source: []const u8) bool {
        _ = self;
        const trimmed = std.mem.trim(u8, source, " \t\r\n");
        return !std.mem.endsWith(u8, trimmed, ";");
    }

    fn printValue(self: *Interpreter, value: Value) !void {
        const stdout = std.io.getStdOut().writer();
        switch (value) {
            .Int => |n| try stdout.print("{d}", .{n}),
            .Float => |n| try stdout.print("{d:.1}", .{n}),
            .String => |s| {
                try stdout.print("{s}", .{s});
            },
            .Nothing => try stdout.print("nothing", .{}),
            .True => try stdout.print("true", .{}),
            .False => try stdout.print("false", .{}),
            .Array => |arr| {
                try stdout.print("[", .{});
                var i: i64 = 0;
                for (arr) |element| {
                    if (i != 0) try stdout.print(", ", .{});
                    try self.printValue(element); // Ensure recursive call to print nested arrays
                    i += 1;
                }
                try stdout.print("]", .{});
            },
            .And => try stdout.print("and", .{}),
            .Or => try stdout.print("or", .{}),
        }
    }

    pub fn debugPrintState(self: *Interpreter) !void {
        const stdout = std.io.getStdOut().writer();
        var it = self.getCurrentScope().iterator();
        while (it.next()) |entry| {
            try stdout.print("  {s} = ", .{entry.key_ptr.*});
            try self.printValue(entry.value_ptr.value);
            try stdout.print("\n", .{});
        }
    }
};
