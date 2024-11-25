const std = @import("std");
const ast = @import("ast.zig");
const token = @import("token.zig");
const ReportingModule = @import("reporting.zig");
const ErrorList = ReportingModule.ErrorList;

pub const Interpreter = struct {
    allocator: std.mem.Allocator,
    variables: std.StringHashMap(token.TokenLiteral),
    debug_enabled: bool,

    pub fn init(allocator: std.mem.Allocator, debug_enabled: bool) !Interpreter {
        return Interpreter{
            .allocator = allocator,
            .variables = std.StringHashMap(token.TokenLiteral).init(allocator),
            .debug_enabled = debug_enabled,
        };
    }

    pub fn deinit(self: *Interpreter) void {
        var it = self.variables.iterator();
        while (it.next()) |entry| {
            self.allocator.free(entry.key_ptr.*);
        }
        self.variables.deinit();
    }

    pub fn interpret(self: *Interpreter, statements: []ast.Stmt) !void {
        for (statements) |stmt| {
            try self.executeStatement(&stmt);
        }
    }

    fn executeStatement(self: *Interpreter, stmt: *const ast.Stmt) !void {
        switch (stmt.*) {
            .VarDecl => |decl| {
                const value = try self.evaluate(decl.initializer);
                const key = try self.allocator.dupe(u8, decl.name.lexeme);
                try self.variables.put(key, value);
                
                if (self.debug_enabled) {
                    std.debug.print("Declared variable {s} = {any}\n", .{
                        decl.name.lexeme, value,
                    });
                    std.debug.print("Variables hashmap contains: {any}\n", .{self.variables.count()});
                }
            },
            .Expression => |expr| {
                if (self.debug_enabled) {
                    std.debug.print("Before evaluating expression, variables count: {any}\n", .{self.variables.count()});
                }
                const value = try self.evaluate(expr);
                if (self.debug_enabled) {
                    std.debug.print("{any}\n", .{value});
                }
            },
        }
    }

    fn evaluate(self: *Interpreter, expr: ?*ast.Expr) ErrorList!token.TokenLiteral {
        if (expr == null) return error.InvalidExpression;
        const e = expr.?;

        switch (e.*) {
            .Literal => |lit| {
                return lit;
            },
            .Binary => |bin| {
                const left = try self.evaluate(bin.left);
                const right = try self.evaluate(bin.right);

                switch (bin.operator.type) {
                    .PLUS => {
                        if (left == .int and right == .int) {
                            return token.TokenLiteral{ .int = left.int + right.int };
                        }
                        return error.TypeError;
                    },
                    .MINUS => {
                        if (left == .int and right == .int) {
                            return token.TokenLiteral{ .int = left.int - right.int };
                        }
                        return error.TypeError;
                    },
                    .ASTERISK => {
                        if (left == .int and right == .int) {
                            return token.TokenLiteral{ .int = left.int * right.int };
                        }
                        return error.TypeError;
                    },
                    .SLASH => {
                        if (left == .int and right == .int) {
                            if (right.int == 0) return error.DivisionByZero;
                            return token.TokenLiteral{ .int = @divTrunc(left.int, right.int) };
                        }
                        return error.TypeError;
                    },
                    else => return error.InvalidOperator,
                }
            },
            .Unary => |un| {
                const operand = try self.evaluate(un.right);
                switch (un.operator.type) {
                    .MINUS => {
                        if (operand == .int) {
                            return token.TokenLiteral{ .int = -operand.int };
                        }
                        return error.TypeError;
                    },
                    .BANG => {
                        if (operand == .boolean) {
                            return token.TokenLiteral{ .boolean = !operand.boolean };
                        }
                        return error.TypeError;
                    },
                    else => return error.InvalidOperator,
                }
            },
            .Variable => |var_token| {
                if (self.debug_enabled) {
                    std.debug.print("Looking up variable: '{s}' (len: {})\n", .{var_token.lexeme, var_token.lexeme.len});
                    var it = self.variables.iterator();
                    while (it.next()) |entry| {
                        std.debug.print("Stored key: '{s}' (len: {}), value: {any}\n", .{
                            entry.key_ptr.*, 
                            entry.key_ptr.*.len,
                            entry.value_ptr.*
                        });
                    }
                }
                if (self.variables.contains(var_token.lexeme)) {
                    const value = self.variables.get(var_token.lexeme).?;
                    return value;
                }
                return error.VariableNotFound;
            },
            .Assignment => |assign| {
                const value = try self.evaluate(assign.value);
                const key = try self.allocator.dupe(u8, assign.name.lexeme);
                try self.variables.put(key, value);
                return value;
            },
        }
    }
};
