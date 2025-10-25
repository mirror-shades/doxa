const std = @import("std");
pub const Token = @import("../types/token.zig").Token;
pub const TokenLiteral = @import("../types/types.zig").TokenLiteral;
const Reporting = @import("../utils/reporting.zig");
const Location = @import("../utils/reporting.zig").Location;
const Reporter = @import("../utils/reporting.zig").Reporter;
const HIRType = @import("../codegen/hir/soxa_types.zig").HIRType;

var next_node_id: NodeId = 0;

pub const NodeId = u32;

pub fn generateNodeId() NodeId {
    defer next_node_id += 1;
    return next_node_id;
}

pub const SourceSpan = struct {
    location: Location,

    pub fn fromToken(token: Token) SourceSpan {
        return .{
            .location = .{
                .file = token.file,
                .range = .{
                    .start_line = @intCast(token.line),
                    .start_col = token.column,
                    .end_line = @intCast(token.line),
                    .end_col = token.column + token.lexeme.len,
                },
            },
        };
    }

    pub fn merge(start: SourceSpan, end: SourceSpan) SourceSpan {
        return .{
            .location = .{
                .file = start.location.file,
                .range = .{
                    .start_line = start.location.range.start_line,
                    .start_col = start.location.range.start_col,
                    .end_line = end.location.range.end_line,
                    .end_col = end.location.range.end_col,
                },
            },
        };
    }
};

pub const StructField = struct {
    name: Token,
    type_expr: *TypeExpr,
    is_public: bool = false,

    pub fn deinit(self: *StructField, allocator: std.mem.Allocator) void {
        self.type_expr.deinit(allocator);
        allocator.destroy(self.type_expr);
    }
};

pub const StructLiteralField = struct {
    name: Token,
    value: *Expr,

    pub fn deinit(self: *StructLiteralField, allocator: std.mem.Allocator) void {
        self.value.deinit(allocator);
        allocator.destroy(self.value);
    }
};

pub const StructInstanceField = struct {
    name: Token,
    value: *Expr,

    pub fn deinit(self: *StructInstanceField, allocator: std.mem.Allocator) void {
        self.value.deinit(allocator);
        allocator.destroy(self.value);
    }
};

pub const MapField = struct {
    name: Token,
    type_expr: *TypeExpr,

    pub fn deinit(self: *MapField, allocator: std.mem.Allocator) void {
        self.type_expr.deinit(allocator);
        allocator.destroy(self.type_expr);
    }
};

pub const MapEntry = struct {
    key: *Expr,
    value: *Expr,

    pub fn deinit(self: *MapEntry, allocator: std.mem.Allocator) void {
        self.key.deinit(allocator);
        allocator.destroy(self.key);
        self.value.deinit(allocator);
        allocator.destroy(self.value);
    }
};

pub const StructMethod = struct {
    name: Token,
    params: []FunctionParam,
    return_type_info: TypeInfo,
    body: []Stmt,
    is_public: bool = false,
    is_static: bool = false,

    pub fn deinit(self: *StructMethod, allocator: std.mem.Allocator) void {
        allocator.free(self.params);
        for (self.body) |*stmt| {
            stmt.deinit(allocator);
        }
        allocator.free(self.body);
    }
};

pub const StructDecl = struct {
    name: Token,
    fields: []*StructField,
    methods: []*StructMethod = &[_]*StructMethod{},
    is_public: bool = false,

    pub fn deinit(self: *StructDecl, allocator: std.mem.Allocator) void {
        for (self.fields) |field| {
            field.deinit(allocator);
            allocator.destroy(field);
        }
        allocator.free(self.fields);

        for (self.methods) |method| {
            method.deinit(allocator);
            allocator.destroy(method);
        }
        allocator.free(self.methods);
    }
};

pub const FunctionParam = struct {
    name: Token,
    type_expr: ?*TypeExpr,
    default_value: ?*Expr = null,
    is_alias: bool = false,

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

pub const Parameter = struct {
    name: Token,
    type_expr: ?*TypeExpr,

    pub fn deinit(self: *Parameter, allocator: std.mem.Allocator) void {
        if (self.type_expr) |type_expr| {
            type_expr.deinit(allocator);
            allocator.destroy(type_expr);
        }
    }
};

pub const FunctionType = struct {
    params: []TypeInfo,
    return_type: *TypeInfo,
    param_aliases: ?[]bool = null,
};

pub const ASTVisitor = struct {
    visitExpr: fn (self: *ASTVisitor, expr: *Expr) anyerror!void,
    visitStmt: fn (self: *ASTVisitor, stmt: *Stmt) anyerror!void,
    visitTypeExpr: fn (self: *ASTVisitor, type_expr: *TypeExpr) anyerror!void,

    enterScope: ?fn (self: *ASTVisitor) anyerror!void = null,
    exitScope: ?fn (self: *ASTVisitor) anyerror!void = null,
};

pub const Base = struct {
    id: NodeId,
    span: ?SourceSpan,

    pub fn location(self: *const Base) Location {
        return if (self.span) |span| span.location else Location{
            .file = "",
            .range = .{
                .start_line = 0,
                .start_col = 0,
                .end_line = 0,
                .end_col = 0,
            },
        };
    }
};

pub const VarDecl = struct {
    name: Token,
    initializer: ?*Expr,
    type_info: TypeInfo,
    is_public: bool = false,
};

pub const EnumDecl = struct {
    name: Token,
    variants: []const Token,
    is_public: bool = false,
};

pub const Stmt = struct {
    base: Base,
    data: Data,

    pub const Data = union(enum) {
        Expression: ?*Expr,
        VarDecl: struct {
            name: Token,
            type_info: TypeInfo,
            initializer: ?*Expr,
            is_public: bool = false,
        },
        Block: []Stmt,
        FunctionDecl: struct {
            name: Token,
            params: []FunctionParam,
            return_type_info: TypeInfo,
            body: []Stmt,
            is_entry: bool = false,
            is_public: bool = false,
            defining_module: ?[]const u8 = null,
        },
        MapDecl: struct {
            name: Token,
            fields: []*MapField,
            is_public: bool = false,
        },
        Return: struct {
            value: ?*Expr,
            type_info: TypeInfo,
        },
        EnumDecl: EnumDecl,
        MapLiteral: []*MapEntry,
        Module: struct {
            name: Token,
            imports: []const ImportInfo,
        },
        Import: ImportInfo,
        Path: []const u8,
        Continue: void,
        Break: void,
        Assert: struct {
            condition: *Expr,
            location: Location,
            message: ?*Expr = null,
        },
        Cast: struct {
            value: *Expr,
            target_type: *TypeExpr,
            then_branch: ?*Expr = null,
            else_branch: ?*Expr,
        },
    };

    pub fn getBase(self: *Stmt) *Base {
        return &self.base;
    }

    pub fn accept(self: *Stmt, visitor: *ASTVisitor) anyerror!void {
        try visitor.visitStmt(self);
    }

    pub fn deinit(self: *Stmt, allocator: std.mem.Allocator) void {
        switch (self.data) {
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
            .FunctionDecl => |*f| {
                allocator.free(f.params);
                for (f.body) |*stmt| {
                    stmt.deinit(allocator);
                }
                allocator.free(f.body);
            },
            .EnumDecl => |decl| {
                allocator.free(decl.variants);
            },
            .MapDecl => |*m| {
                for (m.fields) |field| {
                    field.deinit(allocator);
                    allocator.destroy(field);
                }
                allocator.free(m.fields);
            },
            .MapLiteral => |entries| {
                for (entries) |entry| {
                    entry.deinit(allocator);
                    allocator.destroy(entry);
                }
                allocator.free(entries);
            },
            .Assert => |*a| {
                a.condition.deinit(allocator);
                allocator.destroy(a.condition);
                if (a.message) |msg| {
                    msg.deinit(allocator);
                    allocator.destroy(msg);
                }
            },
            .Module => {},
            .Import => {},
            .Path => {},
            .Continue => {},
            .Break => {},
            .Cast => |*c| {
                c.value.deinit(allocator);
                allocator.destroy(c.value);
                c.target_type.deinit(allocator);
                allocator.destroy(c.target_type);
                if (c.then_branch) |then_expr| {
                    then_expr.deinit(allocator);
                    allocator.destroy(then_expr);
                }
                if (c.else_branch) |else_expr| {
                    else_expr.deinit(allocator);
                    allocator.destroy(else_expr);
                }
            },
        }
    }
};

pub const Binary = struct {
    left: ?*Expr,
    operator: Token,
    right: ?*Expr,
};

pub const Unary = struct {
    operator: Token,
    right: ?*Expr,
};

pub const If = struct {
    condition: ?*Expr,
    then_branch: ?*Expr,
    else_branch: ?*Expr,
};

pub const Assignment = struct {
    name: Token,
    value: ?*Expr,
    target_context: ?VariableRef = null,
};

pub const CallArgument = struct {
    expr: *Expr,
    is_alias: bool = false,
};

pub const CompoundAssignment = struct {
    name: Token,
    operator: Token,
    value: ?*Expr,
};

pub const Index = struct {
    array: *Expr,
    index: *Expr,
};

pub const Logical = struct {
    left: *Expr,
    operator: Token,
    right: *Expr,
};

pub const Loop = struct {
    var_decl: ?*Stmt = null,
    condition: ?*Expr = null,
    step: ?*Expr = null,
    body: *Expr,
};

pub const PeekExpr = struct {
    expr: *Expr,
    location: Location,
    variable_name: ?[]const u8,
    field_name: ?[]const u8 = null,
};

pub const FormatTemplate = struct {
    parts: []FormatPart,

    pub fn deinit(self: *FormatTemplate, allocator: std.mem.Allocator) void {
        for (self.parts) |*part| {
            part.deinit(allocator);
        }
        allocator.free(self.parts);
    }
};

pub const FormatPart = union(enum) {
    String: []const u8,
    Expression: *Expr,

    pub fn deinit(self: *FormatPart, allocator: std.mem.Allocator) void {
        switch (self.*) {
            .String => |str| {
                allocator.free(str);
            },
            .Expression => |expr| {
                expr.deinit(allocator);
                allocator.destroy(expr);
            },
        }
    }
};

pub const PrintExpr = struct {
    expr: ?*Expr,

    format_template: ?*FormatTemplate,

    format_parts: ?[]const []const u8,
    arguments: ?[]const *Expr,
    placeholder_indices: ?[]const u32,

    pub fn deinit(self: *PrintExpr, allocator: std.mem.Allocator) void {
        if (self.expr) |expr| {
            expr.deinit(allocator);
            allocator.destroy(expr);
        }

        if (self.format_template) |template| {
            template.deinit(allocator);
            allocator.destroy(template);
        }

        if (self.format_parts) |parts| {
            for (parts) |part| {
                allocator.free(part);
            }
            allocator.free(parts);
        }

        if (self.arguments) |args| {
            for (args) |arg| {
                arg.deinit(allocator);
                allocator.destroy(arg);
            }
            allocator.free(args);
        }

        if (self.placeholder_indices) |indices| {
            allocator.free(indices);
        }
    }
};

pub const FieldAccess = struct {
    object: *Expr,
    field: Token,
};

pub const MatchExpr = struct {
    value: *Expr,
    cases: []MatchCase,
};

pub const MatchCase = struct {
    pattern: Token,
    body: *Expr,
};

pub const Expr = struct {
    base: Base,
    data: Data,

    pub const Data = union(enum) {
        Literal: TokenLiteral,
        Binary: Binary,
        Unary: Unary,
        Peek: PeekExpr,
        Print: PrintExpr,
        PeekStruct: struct {
            expr: *Expr,
            location: Location,
            variable_name: ?[]const u8,
        },
        Input: struct {
            prompt: Token,
        },
        Variable: Token,
        EnumCase: Token,
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
        FunctionCall: struct {
            callee: *Expr,
            arguments: []CallArgument,
            call_context: ?FunctionCallRef = null,
        },
        Logical: Logical,
        FieldAccess: FieldAccess,
        StructDecl: StructDecl,
        StructLiteral: struct {
            name: Token,
            fields: []const *StructInstanceField,
        },
        FieldAssignment: struct {
            object: *Expr,
            field: Token,
            value: *Expr,
        },
        Exists: struct {
            variable: Token,
            array: *Expr,
            condition: *Expr,
        },
        ForAll: struct {
            variable: Token,
            array: *Expr,
            condition: *Expr,
        },
        ArrayType: struct {
            element_type: *TypeExpr,
            size: ?*Expr = null,
        },
        Match: MatchExpr,
        EnumDecl: struct {
            name: Token,
            variants: []Token,
            is_public: bool = false,
        },
        EnumMember: Token,
        DefaultArgPlaceholder: void,

        BuiltinCall: struct {
            function: Token,
            arguments: []const *Expr,
        },

        Map: []MapEntry,

        InternalCall: struct {
            receiver: *Expr,
            method: Token,
            arguments: []const *Expr,
        },

        Increment: *Expr,
        Decrement: *Expr,

        CompoundAssign: CompoundAssignment,
        Assert: struct {
            condition: *Expr,
            location: Location,
            message: ?*Expr = null,
        },
        Cast: struct {
            value: *Expr,
            target_type: *TypeExpr,
            then_branch: ?*Expr = null,
            else_branch: ?*Expr,
        },
        ReturnExpr: struct { value: ?*Expr },
        Break: void,
        TypeExpr: *TypeExpr,
        Loop: Loop,
        This: void,
        Range: struct {
            start: *Expr,
            end: *Expr,
        },
    };

    pub fn getBase(self: *Expr) *Base {
        return &self.base;
    }

    pub fn accept(self: *Expr, visitor: *ASTVisitor) anyerror!void {
        try visitor.visitExpr(self);
    }

    pub fn deinit(self: *Expr, allocator: std.mem.Allocator) void {
        switch (self.data) {
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
            .FunctionCall => |*c| {
                c.callee.deinit(allocator);
                allocator.destroy(c.callee);
                for (c.arguments) |arg| {
                    arg.expr.deinit(allocator);
                    allocator.destroy(arg.expr);
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
            .Variable => {},
            .EnumCase => {},
            .Literal => |lit| {
                switch (lit) {
                    .string => |str| allocator.free(str),
                    .byte => {},
                    else => {},
                }
            },
            .Logical => |*l| {
                l.left.deinit(allocator);
                allocator.destroy(l.left);
                l.right.deinit(allocator);
                allocator.destroy(l.right);
            },
            .Increment => |*i| {
                i.*.deinit(allocator);
                allocator.destroy(i);
            },
            .Decrement => |*i| {
                i.*.deinit(allocator);
                allocator.destroy(i);
            },
            .Peek => |i| {
                i.expr.deinit(allocator);
                allocator.destroy(i.expr);
            },
            .PeekStruct => |peek| {
                peek.expr.deinit(allocator);
                allocator.destroy(peek.expr);
            },
            .Print => |*pm| {
                pm.deinit(allocator);
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
                for (s.methods) |method| {
                    method.deinit(allocator);
                    allocator.destroy(method);
                }
                allocator.free(s.methods);
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
            .EnumMember => {},
            .DefaultArgPlaceholder => {},
            .BuiltinCall => |*bc| {
                for (bc.arguments) |arg| {
                    arg.deinit(allocator);
                    allocator.destroy(arg);
                }
                allocator.free(bc.arguments);
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
            .InternalCall => |*m| {
                m.receiver.deinit(allocator);
                allocator.destroy(m.receiver);
                for (m.arguments) |arg| {
                    arg.deinit(allocator);
                    allocator.destroy(arg);
                }
                allocator.free(m.arguments);
            },
            .CompoundAssign => |*ca| {
                if (ca.value) |value| {
                    value.deinit(allocator);
                    allocator.destroy(value);
                }
            },
            .Input => {},
            .Assert => |*a| {
                a.condition.deinit(allocator);
                allocator.destroy(a.condition);
                if (a.message) |msg| {
                    msg.deinit(allocator);
                    allocator.destroy(msg);
                }
            },
            .ReturnExpr => |*r| {
                if (r.value) |value| {
                    value.deinit(allocator);
                    allocator.destroy(value);
                }
            },
            .Break => {},
            .TypeExpr => |type_expr| {
                type_expr.deinit(allocator);
                allocator.destroy(type_expr);
            },
            .Cast => |*c| {
                c.value.deinit(allocator);
                allocator.destroy(c.value);
                c.target_type.deinit(allocator);
                allocator.destroy(c.target_type);
                if (c.else_branch) |else_expr| {
                    else_expr.deinit(allocator);
                    allocator.destroy(else_expr);
                }
            },
            .Loop => |*l| {
                if (l.var_decl) |vd| {
                    switch (vd.data) {
                        .VarDecl => |*decl| {
                            if (decl.initializer) |init| {
                                init.deinit(allocator);
                                allocator.destroy(init);
                            }
                        },
                        .Expression => |maybe_expr| {
                            if (maybe_expr) |e| {
                                e.deinit(allocator);
                                allocator.destroy(e);
                            }
                        },
                        else => {},
                    }
                    allocator.destroy(vd);
                }
                if (l.condition) |cond| {
                    cond.deinit(allocator);
                    allocator.destroy(cond);
                }
                if (l.step) |step_expr| {
                    step_expr.deinit(allocator);
                    allocator.destroy(step_expr);
                }
                l.body.deinit(allocator);
                allocator.destroy(l.body);
            },
            .This => {},
            .Range => |*r| {
                r.start.deinit(allocator);
                allocator.destroy(r.start);
                r.end.deinit(allocator);
                allocator.destroy(r.end);
            },
        }
    }
};

pub const Type = enum {
    Int,
    Byte,
    Float,
    String,
    Tetra,
    Array,
    Function,
    Struct,
    Enum,
    Custom,
    Map,
    Nothing,
    Union,
};

pub const TypeInfo = struct {
    base: Type,
    custom_type: ?[]const u8 = null,
    is_mutable: bool = true,
    array_type: ?*TypeInfo = null,
    struct_fields: ?[]StructFieldType = null,
    function_type: ?*FunctionType = null,
    element_type: ?Type = null,
    variants: ?[][]const u8 = null,
    array_size: ?usize = null,
    union_type: ?*UnionType = null,
    map_key_type: ?*TypeInfo = null,
    map_value_type: ?*TypeInfo = null,

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
        if (self.union_type) |union_type| {
            for (union_type.types) |type_info| {
                type_info.deinit(allocator);
                allocator.destroy(type_info);
            }
            allocator.free(union_type.types);
            allocator.destroy(union_type);
        }
        if (self.map_key_type) |key_type| {
            key_type.deinit(allocator);
            allocator.destroy(key_type);
        }
        if (self.map_value_type) |value_type| {
            value_type.deinit(allocator);
            allocator.destroy(value_type);
        }
    }

    pub fn inferFrom(self: *TypeInfo, value: TokenLiteral) void {
        self.base = switch (value) {
            .int => .Int,
            .byte => .Byte,
            .float => .Float,
            .string => .String,
            .tetra => .Tetra,
            .nothing => .Nothing,
            .array => .Array,
            .struct_value => .Struct,
            .function => .Function,
            .enum_variant => .Enum,
            .map => .Map,
        };
    }
};

pub const StructFieldType = struct {
    name: []const u8,
    type_info: *TypeInfo,
};

pub const UnionType = struct {
    types: []const *TypeInfo,
    current_type_index: ?u32 = null,

    pub fn getCurrentType(self: *const UnionType) ?*TypeInfo {
        return if (self.current_type_index) |idx| self.types[idx] else null;
    }

    pub fn setCurrentType(self: *UnionType, type_index: u32) void {
        self.current_type_index = type_index;
    }
};

pub const BasicType = enum {
    Integer,
    Byte,
    Float,
    String,
    Tetra,
    Nothing,
};

pub const ArrayType = struct {
    element_type: *TypeExpr,
    size: ?*Expr = null,
};

pub const TypeExpr = struct {
    base: Base,
    data: Data,

    pub const Data = union(enum) {
        Basic: BasicType,
        Custom: Token,
        Array: ArrayType,
        Struct: []*StructField,
        Enum: []const []const u8,
        Union: []*TypeExpr,
    };

    pub fn getBase(self: *TypeExpr) *Base {
        return &self.base;
    }

    pub fn accept(self: *TypeExpr, visitor: *ASTVisitor) anyerror!void {
        try visitor.visitTypeExpr(self);
    }

    pub fn deinit(self: *TypeExpr, allocator: std.mem.Allocator) void {
        switch (self.data) {
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
            .Union => |types| {
                for (types) |type_expr| {
                    type_expr.deinit(allocator);
                    allocator.destroy(type_expr);
                }
                allocator.free(types);
            },
            else => {},
        }
    }
};

pub const ImportType = enum {
    Module,
    Specific,
};

pub const ImportInfo = struct {
    import_type: ImportType,
    module_path: []const u8,
    namespace_alias: ?[]const u8 = null,
    specific_symbols: ?[][]const u8 = null,
    specific_symbol: ?[]const u8 = null,
};

pub const ModuleSymbol = struct {
    name: []const u8,
    kind: enum { Function, Variable, Struct, Enum },
    is_public: bool,
    stmt_index: usize,
};

pub const ModuleInfo = struct {
    name: []const u8,
    imports: []const ImportInfo,
    ast: ?*Expr = null,
    file_path: []const u8,
    symbols: ?std.StringHashMap(ModuleSymbol) = null,

    pub fn hasPublicSymbol(self: *const ModuleInfo, symbol_name: []const u8) bool {
        if (self.symbols) |symbols| {
            if (symbols.get(symbol_name)) |symbol| {
                return symbol.is_public;
            }
        }
        return false;
    }

    pub fn hasSymbol(self: *const ModuleInfo, symbol_name: []const u8) bool {
        if (self.symbols) |symbols| {
            return symbols.contains(symbol_name);
        }
        return false;
    }

    pub fn getSymbol(self: *const ModuleInfo, symbol_name: []const u8) ?ModuleSymbol {
        if (self.symbols) |symbols| {
            return symbols.get(symbol_name);
        }
        return null;
    }

    pub fn deinit(self: *ModuleInfo, allocator: std.mem.Allocator) void {
        allocator.free(self.imports);
        if (self.symbols) |*symbols| {
            symbols.deinit();
        }
    }
};

pub fn createExpr(allocator: std.mem.Allocator, data: Expr.Data, span: SourceSpan) !*Expr {
    const expr = try allocator.create(Expr);
    expr.* = .{
        .base = .{
            .id = generateNodeId(),
            .span = span,
        },
        .data = data,
    };
    return expr;
}

pub fn createFormatTemplate(allocator: std.mem.Allocator, parts: []FormatPart) !*FormatTemplate {
    const template = try allocator.create(FormatTemplate);
    template.* = .{
        .parts = parts,
    };
    return template;
}

pub fn createStringPart(allocator: std.mem.Allocator, text: []const u8) !FormatPart {
    const owned_text = try allocator.dupe(u8, text);
    return FormatPart{ .String = owned_text };
}

pub fn createExpressionPart(expr: *Expr) FormatPart {
    return FormatPart{ .Expression = expr };
}

pub fn typeInfoFromExpr(allocator: std.mem.Allocator, type_expr: ?*TypeExpr) !*TypeInfo {
    const type_info = try allocator.create(TypeInfo);
    errdefer allocator.destroy(type_info);

    if (type_expr == null) {
        type_info.* = TypeInfo{ .base = .Nothing };
        return type_info;
    }

    type_info.* = switch (type_expr.?.data) {
        .Basic => |basic| switch (basic) {
            .Integer => TypeInfo{ .base = .Int },
            .Byte => TypeInfo{ .base = .Byte },
            .Float => TypeInfo{ .base = .Float },
            .String => TypeInfo{ .base = .String },
            .Tetra => TypeInfo{ .base = .Tetra },
            .Nothing => TypeInfo{ .base = .Nothing },
        },
        .Array => |array| blk: {
            const element_type = try typeInfoFromExpr(allocator, array.element_type);

            var array_size: ?usize = null;
            if (array.size) |size_expr| {
                if (size_expr.data == .Literal) {
                    switch (size_expr.data.Literal) {
                        .int => |i| array_size = @intCast(i),
                        else => {},
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
        .Custom => |custom_token| TypeInfo{ .base = .Custom, .custom_type = custom_token.lexeme },
        .Enum => TypeInfo{ .base = .Nothing },
        .Union => |types| blk: {
            var union_types = try allocator.alloc(*TypeInfo, types.len);
            errdefer allocator.free(union_types);

            for (types, 0..) |union_type_expr, i| {
                union_types[i] = try typeInfoFromExpr(allocator, union_type_expr);
            }

            const union_type = try allocator.create(UnionType);
            union_type.* = .{
                .types = union_types,
                .current_type_index = 0,
            };

            break :blk TypeInfo{
                .base = .Union,
                .union_type = union_type,
            };
        },
    };

    return type_info;
}

pub fn typeInfoFromHIRType(allocator: std.mem.Allocator, hir_type: HIRType) !*TypeInfo {
    const type_info = try allocator.create(TypeInfo);
    errdefer allocator.destroy(type_info);

    switch (hir_type) {
        .Int => type_info.* = .{ .base = .Int },
        .Byte => type_info.* = .{ .base = .Byte },
        .Float => type_info.* = .{ .base = .Float },
        .String => type_info.* = .{ .base = .String },
        .Tetra => type_info.* = .{ .base = .Tetra },
        .Nothing => type_info.* = .{ .base = .Nothing },

        .Array => |elem_ptr| {
            const elem_ti = try typeInfoFromHIRType(allocator, elem_ptr.*);
            type_info.* = .{ .base = .Array, .array_type = elem_ti };
        },

        .Map => {
            type_info.* = .{ .base = .Map };
        },

        .Struct => type_info.* = .{ .base = .Struct },

        .Enum => |_| {
            type_info.* = .{
                .base = .Enum,
                .custom_type = "ValueError",
            };
        },

        .Function => {
            type_info.* = .{ .base = .Function };
        },

        .Union => |member_ptrs| {
            var member_types = try allocator.alloc(*TypeInfo, member_ptrs.len);
            for (member_ptrs, 0..) |member_ptr, i| {
                member_types[i] = try typeInfoFromHIRType(allocator, member_ptr.*);
            }

            const ut = try allocator.create(UnionType);
            ut.* = .{
                .types = member_types,
                .current_type_index = null,
            };

            type_info.* = .{ .base = .Union, .union_type = ut };
        },

        .Unknown => type_info.* = .{ .base = .Nothing },
        .Poison => type_info.* = .{ .base = .Nothing },
    }

    return type_info;
}

pub const VariableRef = struct {
    token: Token,
    module_context: ?[]const u8 = null,
    scope_depth: u32 = 0,
    resolution_kind: ResolutionKind,

    pub const ResolutionKind = enum {
        Local,
        ModuleGlobal,
        ImportedModule,
        ImportedSymbol,
        Unresolved,
    };
};

pub const FunctionCallRef = struct {
    name: []const u8,
    call_kind: CallKind,
    target_module: ?[]const u8 = null,

    pub const CallKind = enum {
        LocalFunction,
        ModuleFunction,
        Unresolved,
    };
};

pub fn createVariableRef(token: Token) VariableRef {
    return VariableRef{
        .token = token,
        .resolution_kind = .Unresolved,
    };
}

pub fn createResolvedVariableRef(token: Token, module_context: ?[]const u8, scope_depth: u32, kind: VariableRef.ResolutionKind) VariableRef {
    return VariableRef{
        .token = token,
        .module_context = module_context,
        .scope_depth = scope_depth,
        .resolution_kind = kind,
    };
}

pub fn createFunctionCallRef(name: []const u8, kind: FunctionCallRef.CallKind, target_module: ?[]const u8) FunctionCallRef {
    return FunctionCallRef{
        .name = name,
        .call_kind = kind,
        .target_module = target_module,
    };
}
