const std = @import("std");
const token = @import("token.zig");

pub const Binary = struct {
    left: ?*Expr,
    operator: token.Token,
    right: ?*Expr,
};

pub const Unary = struct {
    operator: token.Token,
    right: ?*Expr,
};

pub const If = struct {
    condition: ?*Expr,
    then_branch: ?*Expr,
    else_branch: ?*Expr,
};

pub const Expr = union(enum) {
    Literal: token.TokenLiteral,
    Binary: Binary,
    Unary: Unary,
    Print: ?*Expr,
    Variable: token.Token,
    Assignment: Assignment,
    Grouping: ?*Expr,
    If: If,
    Block: struct {
        statements: []Stmt,
        value: ?*Expr,
    },
    Array: []const *Expr,
    Struct: []*StructLiteralField,
    Index: Index,
    IndexAssign: struct {
        array: *Expr,
        index: *Expr,
        value: *Expr,
    },
    Call: struct {
        callee: *Expr,
        arguments: []const *Expr,
    },
    Logical: Logical,
    Function: struct {
        name: token.Token,
        params: []FunctionParam,
        return_type_info: TypeInfo,
        body: []Stmt,
    },
    While: WhileExpr,
    For: ForExpr,
    ForEach: ForEachExpr,

    pub fn deinit(self: *Expr, allocator: std.mem.Allocator) void {
        switch (self.*) {
            .Binary => |*b| {
                if (b.left) |left| {
                    left.deinit(allocator);
                    allocator.destroy(left);
                }
                if (b.right) |right| {
                    right.deinit(allocator);
                    allocator.destroy(right);
                }
            },
            .Call => |*c| {
                c.callee.deinit(allocator);
                allocator.destroy(c.callee);
                for (c.arguments) |arg| {
                    arg.deinit(allocator);
                    allocator.destroy(arg);
                }
                allocator.free(c.arguments);
            },
            .Unary => |*u| {
                if (u.right) |right| {
                    right.deinit(allocator);
                    allocator.destroy(right);
                }
            },
            .Grouping => |g| {
                if (g) |expr| {
                    expr.deinit(allocator);
                    allocator.destroy(expr);
                }
            },
            .Index => |*i| {
                i.array.deinit(allocator);
                allocator.destroy(i.array);
                i.index.deinit(allocator);
                allocator.destroy(i.index);
            },
            .IndexAssign => |*i| {
                i.array.deinit(allocator);
                allocator.destroy(i.array);
                i.index.deinit(allocator);
                allocator.destroy(i.index);
                i.value.deinit(allocator);
                allocator.destroy(i.value);
            },
            .Assignment => |*a| {
                if (a.value) |value| {
                    value.deinit(allocator);
                    allocator.destroy(value);
                }
            },
            .Array => |elements| {
                for (elements) |element| {
                    element.deinit(allocator);
                    allocator.destroy(element);
                }
                allocator.free(elements);
            },
            .Struct => |fields| {
                for (fields) |field| {
                    field.value.deinit(allocator);
                    allocator.destroy(field.value);
                    allocator.destroy(field);
                }
                allocator.free(fields);
            },
            .If => |*i| {
                if (i.condition) |condition| {
                    condition.deinit(allocator);
                    allocator.destroy(condition);
                }
                if (i.then_branch) |then_branch| {
                    then_branch.deinit(allocator);
                    allocator.destroy(then_branch);
                }
                if (i.else_branch) |else_branch| {
                    else_branch.deinit(allocator);
                    allocator.destroy(else_branch);
                }
            },
            .Block => |*b| {
                for (b.statements) |*stmt| {
                    stmt.deinit(allocator);
                }
                allocator.free(b.statements);
                if (b.value) |value| {
                    value.deinit(allocator);
                    allocator.destroy(value);
                }
            },
            .Variable => {}, // This doesn't own any memory
            .Literal => |lit| {
                // Add cleanup for string literals
                switch (lit) {
                    .string => |str| allocator.free(str),
                    else => {}, // Other literals don't own memory
                }
            },
            .Logical => |*l| {
                l.left.deinit(allocator);
                allocator.destroy(l.left);
                l.right.deinit(allocator);
                allocator.destroy(l.right);
            },
            .Function => |*f| {
                for (f.params) |*param| {
                    param.deinit(allocator);
                }
                allocator.free(f.params);
                for (f.body) |*stmt| {
                    stmt.deinit(allocator);
                }
                allocator.free(f.body);
            },
            .Print => |p| {
                if (p) |expr| {
                    expr.deinit(allocator);
                    allocator.destroy(expr);
                }
            },
            .While => |*w| {
                w.condition.deinit(allocator);
                allocator.destroy(w.condition);
                w.body.deinit(allocator);
                allocator.destroy(w.body);
            },
            .For => |*f| {
                if (f.initializer) |init| {
                    init.deinit(allocator);
                    allocator.destroy(init);
                }
                if (f.condition) |condition| {
                    condition.deinit(allocator);
                    allocator.destroy(condition);
                }
                if (f.increment) |increment| {
                    increment.deinit(allocator);
                    allocator.destroy(increment);
                }
                f.body.deinit(allocator);
                allocator.destroy(f.body);
            },
            .ForEach => |*f| {
                f.array.deinit(allocator);
                allocator.destroy(f.array);
                for (f.body) |*stmt| {
                    stmt.deinit(allocator);
                }
                allocator.free(f.body);
            },
        }
    }
};

pub const Assignment = struct {
    name: token.Token,
    value: ?*Expr,
};

pub const Type = enum {
    Int,
    Float,
    String,
    Boolean,
    Nothing,
    Dynamic, // For variables without explicit type
    Auto, // For type inference
    Array, // Will need a subtype field
    Struct, // For struct types
    Function, // For function types
    Enum, // For enum types
};

pub const TypeInfo = struct {
    base: Type,
    is_mutable: bool = true,
    array_type: ?*TypeInfo = null, // For arrays, stores the element type
    struct_fields: ?[]StructFieldType = null, // For structs, stores field types
    function_type: ?*FunctionType = null, // Changed to pointer

    pub fn inferFrom(self: *TypeInfo, value: token.TokenLiteral) void {
        if (self.base != .Auto) return;

        self.base = switch (value) {
            .int => .Int,
            .float => .Float,
            .string => .String,
            .boolean => .Boolean,
            .nothing => .Nothing,
            .array => .Array,
            .struct_value => .Struct,
            .function => .Function,
        };
    }
};

pub const StructFieldType = struct {
    name: []const u8,
    type_info: *TypeInfo, // Changed to pointer
};

pub const FunctionType = struct {
    params: []TypeInfo,
    return_type: *TypeInfo, // Changed to pointer
};

pub const VarDecl = struct {
    name: token.Token,
    initializer: ?*Expr,
    type_info: TypeInfo,

    pub fn deinit(self: *@This(), allocator: std.mem.Allocator) void {
        if (self.initializer) |i| {
            i.deinit(allocator);
            allocator.destroy(i);
        }
    }
};

pub const Stmt = union(enum) {
    VarDecl: struct {
        name: token.Token,
        initializer: ?*Expr,
        type_info: TypeInfo,
    },
    Expression: ?*Expr,
    Block: []Stmt,
    Function: struct {
        name: token.Token,
        params: []FunctionParam,
        return_type_info: TypeInfo,
        body: []Stmt,
    },
    Return: struct {
        value: ?*Expr,
        type_info: TypeInfo,
    },

    pub fn deinit(self: *Stmt, allocator: std.mem.Allocator) void {
        switch (self.*) {
            .Expression => |maybe_expr| {
                if (maybe_expr) |expr| {
                    expr.deinit(allocator);
                    allocator.destroy(expr);
                }
            },
            .Function => |*f| {
                allocator.free(f.params);
                for (f.body) |*stmt| {
                    stmt.deinit(allocator);
                }
                allocator.free(f.body);
            },
            .VarDecl => |*v| {
                if (v.initializer) |init| {
                    init.deinit(allocator);
                    allocator.destroy(init);
                }
            },
            .Block => |statements| {
                for (statements) |*stmt| {
                    stmt.deinit(allocator);
                }
                allocator.free(statements);
            },
            .Return => |*r| {
                if (r.value) |value| {
                    value.deinit(allocator);
                    allocator.destroy(value);
                }
            },
        }
    }
};

pub const TypeExpr = union(enum) {
    Basic: BasicType,
    Array: ArrayType,
    Struct: []*StructField,
    Enum: []const []const u8,

    pub fn deinit(self: *TypeExpr, allocator: std.mem.Allocator) void {
        switch (self.*) {
            .Array => |*array| array.element_type.deinit(allocator),
            .Struct => |fields| {
                for (fields) |field| {
                    field.deinit(allocator);
                    allocator.destroy(field);
                }
                allocator.free(fields);
            },
            .Enum => |variants| {
                allocator.free(variants);
            },
            else => {},
        }
    }
};

pub const BasicType = enum {
    Integer,
    Float,
    String,
    Boolean,
    Auto,
};

pub const ArrayType = struct {
    element_type: *TypeExpr,
};

pub const StructField = struct {
    name: token.Token,
    type_expr: *TypeExpr,

    pub fn deinit(self: *StructField, allocator: std.mem.Allocator) void {
        self.type_expr.deinit(allocator);
        allocator.destroy(self.type_expr);
    }
};

pub const StructLiteralField = struct {
    name: token.Token,
    value: *Expr,

    pub fn deinit(self: *StructLiteralField, allocator: std.mem.Allocator) void {
        self.value.deinit(allocator);
        allocator.destroy(self.value);
    }
};

pub const Index = struct {
    array: *Expr,
    index: *Expr,
};

pub const FunctionParam = struct {
    name: token.Token,
    type_info: TypeInfo,

    pub fn deinit(self: *FunctionParam, allocator: std.mem.Allocator) void {
        // Nothing to deinit since all fields are value types
        _ = self;
        _ = allocator;
    }
};

pub const Logical = struct {
    left: *Expr,
    operator: token.Token,
    right: *Expr,
};

pub const Parameter = struct {
    name: token.Token,
    type_expr: ?*TypeExpr,

    pub fn deinit(self: *Parameter, allocator: std.mem.Allocator) void {
        if (self.type_expr) |type_expr| {
            type_expr.deinit(allocator);
            allocator.destroy(type_expr);
        }
    }
};

pub const WhileExpr = struct {
    condition: *Expr,
    body: *Expr,
};

pub const ForExpr = struct {
    initializer: ?*Stmt,
    condition: ?*Expr,
    increment: ?*Expr,
    body: *Expr,
};

pub const ForEachExpr = struct {
    item_name: token.Token,
    array: *Expr,
    body: []Stmt,
};

// Helper function to create TypeInfo from type expression
pub fn typeInfoFromExpr(type_expr: ?*TypeExpr) TypeInfo {
    if (type_expr == null) {
        return TypeInfo{ .base = .Dynamic };
    }

    return switch (type_expr.?.*) {
        .Basic => |basic| switch (basic) {
            .Integer => TypeInfo{ .base = .Int },
            .Float => TypeInfo{ .base = .Float },
            .String => TypeInfo{ .base = .String },
            .Boolean => TypeInfo{ .base = .Boolean },
            .Auto => TypeInfo{ .base = .Auto },
        },
        .Array => |array| TypeInfo{
            .base = .Array,
            .array_type = typeInfoFromExpr(array.element_type),
        },
        else => TypeInfo{ .base = .Dynamic },
    };
}
