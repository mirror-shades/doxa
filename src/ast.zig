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
    Print: PrintExpr,
    Variable: token.Token,
    Assignment: Assignment,
    Grouping: ?*Expr,
    If: If,
    Block: struct {
        statements: []Stmt,
        value: ?*Expr,
    },
    Array: []const *Expr,
    Tuple: []const *Expr,
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
    FieldAccess: FieldAccess,
    StructDecl: StructDecl,
    StructLiteral: struct {
        name: token.Token,
        fields: []const *StructInstanceField,
    },
    FieldAssignment: struct {
        object: *Expr,
        field: token.Token,
        value: *Expr,
    },
    Exists: struct {
        variable: token.Token,
        array: *Expr,
        condition: *Expr,
    },
    ForAll: struct {
        variable: token.Token,
        array: *Expr,
        condition: *Expr,
    },
    ArrayType: struct {
        element_type: ?*TypeExpr = null,
    },
    Match: MatchExpr,
    EnumDecl: struct {
        name: token.Token,
        variants: []token.Token,
    },
    EnumMember: token.Token,
    DefaultArgPlaceholder: void,
    TypeOf: *Expr,

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
                p.expr.deinit(allocator);
                allocator.destroy(p.expr);
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
            .FieldAccess => |*f| {
                f.object.deinit(allocator);
                allocator.destroy(f.object);
            },
            .StructDecl => |*s| {
                for (s.fields) |field| {
                    field.deinit(allocator);
                    allocator.destroy(field);
                }
                allocator.free(s.fields);
            },
            .StructLiteral => |*s| {
                for (s.fields) |field| {
                    field.deinit(allocator);
                    allocator.destroy(field);
                }
                allocator.free(s.fields);
            },
            .FieldAssignment => |*f| {
                f.object.deinit(allocator);
                allocator.destroy(f.object);
                f.value.deinit(allocator);
                allocator.destroy(f.value);
            },
            .Exists => |*e| {
                e.condition.deinit(allocator);
                allocator.destroy(e.condition);
            },
            .ForAll => |*f| {
                f.array.deinit(allocator);
                allocator.destroy(f.array);
                f.condition.deinit(allocator);
                allocator.destroy(f.condition);
            },
            .ArrayType => |*array| {
                if (array.element_type) |element_type| {
                    element_type.deinit(allocator);
                    allocator.destroy(element_type);
                }
            },
            .Match => |*m| {
                m.value.deinit(allocator);
                allocator.destroy(m.value);
                for (m.cases) |*c| {
                    c.body.deinit(allocator);
                    allocator.destroy(c.body);
                }
                allocator.free(m.cases);
            },
            .EnumDecl => |*e| {
                allocator.free(e.variants);
            },
            .EnumMember => {}, // No allocation to free
            .DefaultArgPlaceholder => {}, // Nothing to deallocate
            .TypeOf => |expr| {
                expr.deinit(allocator);
                allocator.destroy(expr);
            },
            .Tuple => |elements| {
                for (elements) |element| {
                    element.deinit(allocator);
                    allocator.destroy(element);
                }
                allocator.free(elements);
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
    Custom, // For custom types
    Exists, // For existential quantifiers
    Forall, // For universal quantifiers
};

pub const TypeInfo = struct {
    base: Type,
    custom_type: ?[]const u8 = null,
    is_dynamic: bool = false,
    is_mutable: bool = true,
    array_type: ?*TypeInfo = null,
    struct_fields: ?[]StructFieldType = null,
    function_type: ?*FunctionType = null,
    element_type: ?Type = null,
    variants: ?[][]const u8 = null,

    pub fn deinit(self: *TypeInfo, allocator: std.mem.Allocator) void {
        if (self.array_type) |array_type| {
            array_type.deinit(allocator);
            allocator.destroy(array_type);
        }
        if (self.struct_fields) |fields| {
            for (fields) |field| {
                field.type_info.deinit(allocator);
                allocator.destroy(field.type_info);
            }
            allocator.free(fields);
        }
        if (self.function_type) |func_type| {
            func_type.return_type.deinit(allocator);
            allocator.destroy(func_type.return_type);
            for (func_type.params) |*param| {
                param.deinit(allocator);
            }
            allocator.free(func_type.params);
            allocator.destroy(func_type);
        }
        if (self.variants) |variants| {
            allocator.free(variants);
        }
    }

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
            .enum_variant => .Enum,
        };
    }
};

pub const StructFieldType = struct {
    name: []const u8,
    type_info: *TypeInfo,
};

pub const FunctionType = struct {
    params: []TypeInfo,
    return_type: *TypeInfo,
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
        self.type_info.deinit(allocator);
    }
};

pub const EnumDecl = struct {
    name: token.Token,
    variants: []const token.Token,
};

pub const Stmt = union(enum) {
    Expression: ?*Expr,
    VarDecl: struct {
        name: token.Token,
        type_info: TypeInfo,
        initializer: ?*Expr,
    },
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
    EnumDecl: EnumDecl,

    pub fn deinit(self: *Stmt, allocator: std.mem.Allocator) void {
        switch (self.*) {
            .Expression => |maybe_expr| {
                if (maybe_expr) |expr| {
                    expr.deinit(allocator);
                    allocator.destroy(expr);
                }
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
            .Function => |*f| {
                allocator.free(f.params);
                for (f.body) |*stmt| {
                    stmt.deinit(allocator);
                }
                allocator.free(f.body);
            },
            .EnumDecl => |decl| {
                allocator.free(decl.variants);
            },
        }
    }
};

pub const TypeExpr = union(enum) {
    Basic: BasicType,
    Custom: token.Token,
    Array: ArrayType,
    Struct: []*StructField,
    Enum: []const []const u8,
    Exists: struct {
        variable: token.Token,
        condition: *TypeExpr,
    },
    Forall: struct {
        variable: token.Token,
        condition: *TypeExpr,
    },

    pub fn deinit(self: *TypeExpr, allocator: std.mem.Allocator) void {
        switch (self.*) {
            .Array => |*array| {
                array.element_type.deinit(allocator);
                allocator.destroy(array.element_type);
            },
            .Struct => |fields| {
                for (fields) |field| {
                    field.type_expr.deinit(allocator);
                    allocator.destroy(field.type_expr);
                    allocator.destroy(field);
                }
                allocator.free(fields);
            },
            .Enum => |variants| {
                allocator.free(variants);
            },
            .Exists => |*e| {
                e.condition.deinit(allocator);
                allocator.destroy(e.condition);
            },
            .Forall => |*f| {
                f.condition.deinit(allocator);
                allocator.destroy(f.condition);
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
    type_expr: ?*TypeExpr,
    default_value: ?*Expr = null,

    pub fn deinit(self: *FunctionParam, allocator: std.mem.Allocator) void {
        if (self.type_expr) |te| {
            te.deinit(allocator);
            allocator.destroy(te);
        }
        if (self.default_value) |dv| {
            dv.deinit(allocator);
            allocator.destroy(dv);
        }
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

pub const Location = struct {
    file: []const u8,
    line: i32,
    column: usize,
};

pub const PrintExpr = struct {
    expr: *Expr,
    location: Location,
};

pub const FieldAccess = struct {
    object: *Expr,
    field: token.Token,
};

pub const StructDecl = struct {
    name: token.Token,
    fields: []*StructField,
};

// Helper function to create TypeInfo from type expression
pub fn typeInfoFromExpr(allocator: std.mem.Allocator, type_expr: ?*TypeExpr) !*TypeInfo {
    const type_info = try allocator.create(TypeInfo);
    errdefer allocator.destroy(type_info);

    if (type_expr == null) {
        type_info.* = TypeInfo{ .base = .Dynamic };
        return type_info;
    }

    type_info.* = switch (type_expr.?.*) {
        .Basic => |basic| switch (basic) {
            .Integer => TypeInfo{ .base = .Int },
            .Float => TypeInfo{ .base = .Float },
            .String => TypeInfo{ .base = .String },
            .Boolean => TypeInfo{ .base = .Boolean },
            .Auto => TypeInfo{ .base = .Auto },
        },
        .Array => |array| blk: {
            const element_type = try typeInfoFromExpr(allocator, array.element_type);
            break :blk TypeInfo{
                .base = .Array,
                .array_type = element_type,
            };
        },
        .Struct => |fields| blk: {
            var struct_fields = try allocator.alloc(StructFieldType, fields.len);
            errdefer allocator.free(struct_fields);

            for (fields, 0..) |field, i| {
                const field_type = try typeInfoFromExpr(allocator, field.type_expr);
                struct_fields[i] = .{
                    .name = field.name.lexeme,
                    .type_info = field_type,
                };
            }
            break :blk TypeInfo{
                .base = .Struct,
                .struct_fields = struct_fields,
            };
        },
        .Custom => TypeInfo{ .base = .Custom },
        .Exists => TypeInfo{ .base = .Exists },
        .Forall => TypeInfo{ .base = .Forall },
        .Enum => TypeInfo{ .base = .Dynamic }, // TODO: Add proper enum support
    };

    return type_info;
}

// Add a new struct for struct instance fields
pub const StructInstanceField = struct {
    name: token.Token,
    value: *Expr,

    pub fn deinit(self: *StructInstanceField, allocator: std.mem.Allocator) void {
        self.value.deinit(allocator);
        allocator.destroy(self.value);
    }
};

pub const MatchExpr = struct {
    value: *Expr,
    cases: []MatchCase,
};

pub const MatchCase = struct {
    pattern: token.Token,
    body: *Expr,
};
