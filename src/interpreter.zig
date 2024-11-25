const std = @import("std");
const ast = @import("ast.zig");
const token = @import("token.zig");
const ReportingModule = @import("reporting.zig");
const ErrorList = ReportingModule.ErrorList;

const StringInterner = struct {
    strings: std.StringHashMap([]const u8),
    allocator: std.mem.Allocator,

    pub fn init(allocator: std.mem.Allocator) StringInterner {
        return StringInterner{
            .strings = std.StringHashMap([]const u8).init(allocator),
            .allocator = allocator,
        };
    }

    pub fn deinit(self: *StringInterner) void {
        var it = self.strings.iterator();
        while (it.next()) |entry| {
            self.allocator.free(entry.key_ptr.*);
        }
        self.strings.deinit();
    }

    pub fn intern(self: *StringInterner, string: []const u8) ![]const u8 {
        if (self.strings.get(string)) |existing| {
            return existing;
        }
        const copy = try self.allocator.dupe(u8, string);
        try self.strings.put(copy, copy);
        return copy;
    }
};

pub const Interpreter = struct {
    allocator: std.mem.Allocator,
    variables: std.StringHashMap(token.TokenLiteral),
    debug_enabled: bool,
    string_interner: StringInterner,

    pub fn init(allocator: std.mem.Allocator, debug_enabled: bool) !Interpreter {
        return Interpreter{
            .allocator = allocator,
            .variables = std.StringHashMap(token.TokenLiteral).init(allocator),
            .debug_enabled = debug_enabled,
            .string_interner = StringInterner.init(allocator),
        };
    }

    pub fn deinit(self: *Interpreter) void {
        self.variables.deinit();
        self.string_interner.deinit();
    }

    pub fn interpret(self: *Interpreter, statements: []ast.Stmt) !void {
        for (statements) |stmt| {
            try self.executeStatement(&stmt);
        }
    }

    pub fn compare(a: anytype, b: anytype) i8 {
        var a_float: f32 = undefined;
        var b_float: f32 = undefined;
        if (@TypeOf(a) == i32 or @TypeOf(b) == i32) {
            a_float = @as(f32, @floatFromInt(a));
            b_float = @as(f32, @floatFromInt(b));
        } else if (@TypeOf(a) == f32 or @TypeOf(b) == f32) {
            a_float = a;
            b_float = b;
        } else {
            return error.InvalidType;
        }
        if (a_float < b_float) return -1;
        if (a_float > b_float) return 1;
        return 0;
    }

    fn executeStatement(self: *Interpreter, stmt: *const ast.Stmt) !void {
        switch (stmt.*) {
            .VarDecl => |decl| {
                const value = try self.evaluate(decl.initializer);
                const key = try self.string_interner.intern(decl.name.lexeme);
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
                    .GREATER => {
                        if (left == .int and right == .int) {
                            return token.TokenLiteral{ .boolean = Interpreter.compare(left.int, right.int) > 0 };
                        }
                        return error.TypeError;
                    },
                    .GREATER_EQUAL => {
                        if (left == .int and right == .int) {
                            return token.TokenLiteral{ .boolean = Interpreter.compare(left.int, right.int) >= 0 };
                        }
                        return error.TypeError;
                    },
                    .LESS => {
                        if (left == .int and right == .int) {
                            return token.TokenLiteral{ .boolean = Interpreter.compare(left.int, right.int) < 0 };
                        }
                        return error.TypeError;
                    },
                    .LESS_EQUAL => {
                        if (left == .int and right == .int) {
                            return token.TokenLiteral{ .boolean = Interpreter.compare(left.int, right.int) <= 0 };
                        }
                        return error.TypeError;
                    },
                    .EQUALITY => {
                        return token.TokenLiteral{ .boolean = Interpreter.compare(left.int, right.int) == 0 };
                    },
                    .BANG_EQUAL => {
                        return token.TokenLiteral{ .boolean = Interpreter.compare(left.int, right.int) != 0 };
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
                    std.debug.print("Looking up variable: '{s}' (len: {})\n", .{ var_token.lexeme, var_token.lexeme.len });
                    var it = self.variables.iterator();
                    while (it.next()) |entry| {
                        std.debug.print("Stored key: '{s}' (len: {}), value: {any}\n", .{ entry.key_ptr.*, entry.key_ptr.*.len, entry.value_ptr.* });
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
                const key = try self.string_interner.intern(assign.name.lexeme);
                try self.variables.put(key, value);
                return value;
            },
            .Grouping => |group| {
                return try self.evaluate(group);
            },
        }
    }
};