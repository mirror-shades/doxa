const std = @import("std");
const Node = @import("ast.zig").Node;
const TokenKind = @import("lexer.zig").TokenKind;

pub const Interpreter = struct {
    allocator: *std.mem.Allocator,
    variables: std.StringHashMap(Variable),

    const Variable = struct {
        value: f64,
        is_mutable: bool,
    };

    pub fn init(allocator: *std.mem.Allocator) Interpreter {
        return Interpreter{
            .allocator = allocator,
            .variables = std.StringHashMap(Variable).init(allocator.*),
        };
    }

    pub fn evaluate(self: *Interpreter, node: *Node) !f64 {
        return self.evalNode(node);
    }

    fn evalNode(self: *Interpreter, node: *Node) !f64 {
        return switch (node.*) {
            .Number => |n| n.value,
            .Binary => |binary| {
                const left = try self.evalNode(binary.left);
                const right = try self.evalNode(binary.right);
                return switch (binary.operator) {
                    .Plus => left + right,
                    .Minus => left - right,
                    .Star => left * right,
                    .Slash => left / right,
                    .NumberType, .FloatType, .StringType, 
                    .BoolType, .Colon,
                    .LeftParen, .RightParen, .Identifier, 
                    .Equal, .Number, .EOF => error.InvalidOperator,
                    else => unreachable,
                };
            },
            .Declaration => |decl| {
                const value = try self.evalNode(decl.value);
                try self.variables.put(decl.name, .{
                    .value = value,
                    .is_mutable = decl.is_mutable,
                });
                return value;
            },
            .Assignment => |assignment| {
                if (self.variables.get(assignment.name)) |var_info| {
                    if (!var_info.is_mutable) {
                        return error.ConstAssignment;
                    }
                    const value = try self.evalNode(assignment.value);
                    try self.variables.put(assignment.name, .{
                        .value = value,
                        .is_mutable = true,
                    });
                    return value;
                } else {
                    return error.UndefinedVariable;
                }
            },
            .Variable => |v| {
                if (self.variables.get(v.name)) |var_info| {
                    return var_info.value;
                } else {
                    return error.UndefinedVariable;
                }
            },
            .Print => |print_node| {
                const value = try self.evalNode(print_node.expression);
                const stdout = std.io.getStdOut().writer();
                try stdout.print("{d}\n", .{value});
                return value;
            },
        };
    }

    pub fn shouldPrintResult(self: *const Interpreter, source: []const u8) bool {
        _ = self;
        const trimmed = std.mem.trim(u8, source, " \t\r\n");
        return !std.mem.endsWith(u8, trimmed, ";");
    }
};
