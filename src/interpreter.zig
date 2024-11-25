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

pub const Environment = struct {
    enclosing: ?*Environment,
    values: std.StringHashMap(token.TokenLiteral),
    allocator: std.mem.Allocator,

    pub fn init(allocator: std.mem.Allocator, enclosing: ?*Environment) Environment {
        return Environment{
            .enclosing = enclosing,
            .values = std.StringHashMap(token.TokenLiteral).init(allocator),
            .allocator = allocator,
        };
    }

    pub fn deinit(self: *Environment) void {
        self.values.deinit();
    }

    pub fn define(self: *Environment, name: []const u8, value: token.TokenLiteral) !void {
        try self.values.put(name, value);
    }

    pub fn get(self: *Environment, name: []const u8) ?token.TokenLiteral {
        if (self.values.get(name)) |value| {
            return value;
        }
        if (self.enclosing) |enclosing| {
            return enclosing.get(name);
        }
        return null;
    }

    pub fn assign(self: *Environment, name: []const u8, value: token.TokenLiteral) !void {
        if (self.values.contains(name)) {
            try self.values.put(name, value);
            return;
        }
        if (self.enclosing) |enclosing| {
            try enclosing.assign(name, value);
            return;
        }
        return error.UndefinedVariable;
    }
};

pub const Interpreter = struct {
    allocator: std.mem.Allocator,
    environment: *Environment,
    globals: *Environment,
    debug_enabled: bool,
    string_interner: StringInterner,

    pub fn init(allocator: std.mem.Allocator, debug_enabled: bool) !Interpreter {
        const globals = try allocator.create(Environment);
        globals.* = Environment.init(allocator, null);

        return Interpreter{
            .allocator = allocator,
            .environment = globals,
            .globals = globals,
            .debug_enabled = debug_enabled,
            .string_interner = StringInterner.init(allocator),
        };
    }

    pub fn deinit(self: *Interpreter) void {
        self.environment.deinit();
        self.allocator.destroy(self.globals);
        self.string_interner.deinit();
    }

    pub fn executeBlock(self: *Interpreter, statements: []ast.Stmt, environment: *Environment) ErrorList!void {
        const previous = self.environment;
        self.environment = environment;
        defer self.environment = previous;

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

    pub fn executeStatement(self: *Interpreter, stmt: *const ast.Stmt) ErrorList!void {
        switch (stmt.*) {
            .VarDecl => |decl| {
                const value = try self.evaluate(decl.initializer);
                const key = try self.string_interner.intern(decl.name.lexeme);
                try self.environment.define(key, value);

                if (self.debug_enabled) {
                    std.debug.print("Declared variable {s} = {any}\n", .{
                        decl.name.lexeme, value,
                    });
                }
            },
            .Expression => |expr| {
                if (self.debug_enabled) {
                    std.debug.print("Before evaluating expression\n", .{});
                }
                const value = try self.evaluate(expr);
                if (self.debug_enabled) {
                    std.debug.print("{any}\n", .{value});
                }
            },
            .Block => |statements| {
                var block_env = Environment.init(self.allocator, self.environment);
                defer block_env.deinit();
                try self.executeBlock(statements, &block_env);
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
                    var it = self.environment.values.iterator();
                    while (it.next()) |entry| {
                        std.debug.print("Stored key: '{s}' (len: {}), value: {any}\n", .{ entry.key_ptr.*, entry.key_ptr.*.len, entry.value_ptr.* });
                    }
                }
                if (self.environment.get(var_token.lexeme)) |value| {
                    return value;
                }
                return error.VariableNotFound;
            },
            .Assignment => |assign| {
                const value = try self.evaluate(assign.value);
                const key = try self.string_interner.intern(assign.name.lexeme);
                try self.environment.define(key, value);
                return value;
            },
            .Grouping => |group| {
                return try self.evaluate(group);
            },
            .If => |if_expr| {
                const condition = try self.evaluate(if_expr.condition);
                if (condition != .boolean) {
                    return error.TypeError;
                }

                return if (condition.boolean)
                    try self.evaluate(if_expr.then_branch)
                else
                    try self.evaluate(if_expr.else_branch);
            },
        }
    }
};
