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

pub const MapEntry = struct {
    key: *Expr,
    value: *Expr,
};

pub const Expr = union(enum) {
    Literal: token.TokenLiteral,
    Binary: Binary,
    Unary: Unary,
    Print: PrintExpr,
    Input: struct {
        prompt: token.Token,
    },
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
        is_entry: bool = false,
        is_public: bool = false,
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
        element_type: *TypeExpr,
        size: ?*Expr = null,
    },
    Match: MatchExpr,
    EnumDecl: struct {
        name: token.Token,
        variants: []token.Token,
        is_public: bool = false,
    },
    EnumMember: token.Token,
    DefaultArgPlaceholder: void,
    TypeOf: *Expr,
    Map: []MapEntry,
    MethodCall: struct {
        receiver: *Expr,
        method: token.Token,
        arguments: []const *Expr,
    },
    ArrayPush: struct {
        array: *Expr,
        element: *Expr,
    },
    ArrayLength: struct {
        array: *Expr,
    },
    ArrayPop: struct {
        array: *Expr,
    },
    ArrayIsEmpty: struct {
        array: *Expr,
    },
    ArrayConcat: struct {
        array: *Expr,
        array2: *Expr,
    },
    CompoundAssign: CompoundAssignment,

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
                switch (lit) {
                    .string => |str| allocator.free(str),
                    .u8 => {}, // No cleanup needed for u8
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
                array.element_type.deinit(allocator);
                allocator.destroy(array.element_type);

                // Also clean up the size expression if it exists
                if (array.size) |size| {
                    size.deinit(allocator);
                    allocator.destroy(size);
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
            .Map => |entries| {
                for (entries) |entry| {
                    entry.key.deinit(allocator);
                    allocator.destroy(entry.key);
                    entry.value.deinit(allocator);
                    allocator.destroy(entry.value);
                }
                allocator.free(entries);
            },
            .MethodCall => |*m| {
                m.receiver.deinit(allocator);
                allocator.destroy(m.receiver);
                for (m.arguments) |arg| {
                    arg.deinit(allocator);
                    allocator.destroy(arg);
                }
                allocator.free(m.arguments);
            },
            .ArrayPush => |*ap| {
                ap.array.deinit(allocator);
                allocator.destroy(ap.array);
                ap.element.deinit(allocator);
                allocator.destroy(ap.element);
            },
            .ArrayLength => |*a| {
                a.array.deinit(allocator);
                allocator.destroy(a.array);
            },
            .ArrayPop => |*a| {
                a.array.deinit(allocator);
                allocator.destroy(a.array);
            },
            .ArrayIsEmpty => |*a| {
                a.array.deinit(allocator);
                allocator.destroy(a.array);
            },
            .ArrayConcat => |*a| {
                a.array.deinit(allocator);
                allocator.destroy(a.array);
            },
            .CompoundAssign => |*ca| {
                if (ca.value) |value| {
                    value.deinit(allocator);
                    allocator.destroy(value);
                }
            },
            .Input => {}, // Token doesn't own any memory, so no cleanup needed
        }
    }
};

pub const Assignment = struct {
    name: token.Token,
    value: ?*Expr,
};

pub const Type = enum {
    Int,
    U8,
    Float,
    String,
    Boolean,
    Tetra,
    Array,
    Tuple,
    Function,
    Struct,
    Enum,
    Dynamic,
    Auto,
    Custom,
    Map,
    Nothing,
    Bytes,
    Reference,
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
    array_size: ?usize = null,
    referenced_type: ?*TypeInfo = null,

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
        if (self.referenced_type) |ref_type| {
            ref_type.deinit(allocator);
            allocator.destroy(ref_type);
        }
    }

    pub fn inferFrom(self: *TypeInfo, value: token.TokenLiteral) void {
        if (self.base != .Auto) return;

        self.base = switch (value) {
            .int => .Int,
            .u8 => .U8,
            .float => .Float,
            .string => .String,
            .boolean => .Boolean,
            .tetra => .Tetra,
            .nothing => .Nothing,
            .array => .Array,
            .struct_value => .Struct,
            .function => .Function,
            .enum_variant => .Enum,
            .bytes => .Bytes,
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
    is_public: bool = false,
};

pub const EnumDecl = struct {
    name: token.Token,
    variants: []const token.Token,
    is_public: bool = false,
};

pub const ImportInfo = struct {
    module_path: []const u8,
    namespace_alias: ?[]const u8 = null,
    specific_symbol: ?[]const u8 = null,
};

pub const Stmt = union(enum) {
    Expression: ?*Expr,
    VarDecl: struct {
        name: token.Token,
        type_info: TypeInfo,
        initializer: ?*Expr,
        is_public: bool = false,
    },
    Block: []Stmt,
    Function: struct {
        name: token.Token,
        params: []FunctionParam,
        return_type_info: TypeInfo,
        body: []Stmt,
        is_entry: bool = false,
        is_public: bool = false,
    },
    Return: struct {
        value: ?*Expr,
        type_info: TypeInfo,
    },
    EnumDecl: EnumDecl,
    Map: []MapEntry,
    Try: TryStmt,
    Module: struct {
        name: token.Token,
        imports: []const ImportInfo,
    },
    Import: ImportInfo,
    Path: []const u8,
    Continue: void,
    Break: void,
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
            .Map => |entries| {
                for (entries) |entry| {
                    entry.key.deinit(allocator);
                    allocator.destroy(entry.key);
                    entry.value.deinit(allocator);
                    allocator.destroy(entry.value);
                }
                allocator.free(entries);
            },
            .Try => |*t| {
                for (t.try_body) |*stmt| {
                    stmt.deinit(allocator);
                }
                allocator.free(t.try_body);
                for (t.catch_body) |*stmt| {
                    stmt.deinit(allocator);
                }
                allocator.free(t.catch_body);
            },
            .Module => {},
            .Import => {},
            .Path => {},
            .Continue => {},
            .Break => {},
        }
    }
};

pub const TypeExpr = union(enum) {
    Basic: BasicType,
    Custom: token.Token,
    Array: ArrayType,
    Struct: []*StructField,
    Enum: []const []const u8,

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
            else => {},
        }
    }
};

pub const BasicType = enum {
    Integer,
    U8,
    Float,
    String,
    Boolean,
    Tetra,
    Auto,
};

pub const ArrayType = struct {
    element_type: *TypeExpr,
    size: ?*Expr = null,
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
    variable_name: ?[]const u8,
};

pub const FieldAccess = struct {
    object: *Expr,
    field: token.Token,
};

pub const StructDecl = struct {
    name: token.Token,
    fields: []*StructField,
    is_public: bool = false,
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
            .U8 => TypeInfo{ .base = .U8 },
            .Float => TypeInfo{ .base = .Float },
            .String => TypeInfo{ .base = .String },
            .Boolean => TypeInfo{ .base = .Boolean },
            .Tetra => TypeInfo{ .base = .Tetra },
            .Auto => TypeInfo{ .base = .Auto },
        },
        .Array => |array| blk: {
            const element_type = try typeInfoFromExpr(allocator, array.element_type);

            // Extract array size if present
            var array_size: ?usize = null;
            if (array.size) |size_expr| {
                if (size_expr.* == .Literal) {
                    switch (size_expr.Literal) {
                        .int => |i| array_size = @intCast(i),
                        else => {}, // Only integer literals are supported for array sizes
                    }
                }
            }

            break :blk TypeInfo{
                .base = .Array,
                .array_type = element_type,
                .array_size = array_size,
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

pub const TryStmt = struct {
    try_body: []Stmt,
    catch_body: []Stmt,
    error_var: ?token.Token,
};

pub const ModuleInfo = struct {
    name: []const u8,
    imports: []const ImportInfo,
    ast: ?*Expr = null, // Store the module's AST for reference
};

pub const CompoundAssignment = struct {
    name: token.Token,
    operator: token.Token, // The compound operator (e.g., MINUS_EQUALS)
    value: ?*Expr,
};
