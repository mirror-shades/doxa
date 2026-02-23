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
                .file_uri = token.file_uri,
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
                .file_uri = start.location.file_uri,
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
            .file_uri = null,
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
        ZigDecl: struct {
            name: Token,
            source: []const u8,
        },
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
            key_type: TypeInfo,
            value_type: TypeInfo,
            fields: []*MapField,
            else_value: ?*Expr = null,
            is_public: bool = false,
        },
        Return: struct {
            value: ?*Expr,
            type_info: TypeInfo,
        },
        EnumDecl: EnumDecl,
        MapLiteral: struct {
            entries: []*MapEntry,
            key_type: ?TypeInfo = null,
            value_type: ?TypeInfo = null,
            else_value: ?*Expr = null,
        },
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
            .ZigDecl => |z| {
                allocator.free(z.source);
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
                if (m.else_value) |else_val| {
                    else_val.deinit(allocator);
                    allocator.destroy(else_val);
                }
            },
            .MapLiteral => |*map_literal| {
                for (map_literal.entries) |entry| {
                    entry.deinit(allocator);
                    allocator.destroy(entry);
                }
                allocator.free(map_literal.entries);
                if (map_literal.else_value) |else_val| {
                    else_val.deinit(allocator);
                    allocator.destroy(else_val);
                }
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
    patterns: []Token,
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

        Map: struct {
            entries: []*MapEntry,
            key_type: ?*TypeInfo = null,
            value_type: ?*TypeInfo = null,
        },
        MapLiteral: struct {
            entries: []*MapEntry,
            key_type: ?*TypeInfo = null,
            value_type: ?*TypeInfo = null,
            else_value: ?*Expr = null,
        },

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
        Unreachable: struct {
            keyword: Token,
        },
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
                    allocator.free(c.patterns); // Free the patterns array
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

            .Map => |*map_expr| {
                for (map_expr.entries) |entry| {
                    entry.deinit(allocator);
                    allocator.destroy(entry);
                }
                allocator.free(map_expr.entries);
            },
            .MapLiteral => |*map_literal| {
                for (map_literal.entries) |entry| {
                    entry.key.deinit(allocator);
                    allocator.destroy(entry.key);
                    entry.value.deinit(allocator);
                    allocator.destroy(entry.value);
                }
                allocator.free(map_literal.entries);
                if (map_literal.else_value) |else_val| {
                    else_val.deinit(allocator);
                    allocator.destroy(else_val);
                }
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
            .Unreachable => {},
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

pub const ArrayStorageKind = enum {
    dynamic,
    fixed,
    const_literal,
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
    array_storage: ArrayStorageKind = .dynamic,
    union_type: ?*UnionType = null,
    map_key_type: ?*TypeInfo = null,
    map_value_type: ?*TypeInfo = null,
    map_has_else_value: bool = false,

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

    pub fn initDefault(self: *TypeInfo) void {
        self.* = TypeInfo{ .base = .Nothing };
    }

    pub fn createDefault(allocator: std.mem.Allocator) !*TypeInfo {
        const type_info = try allocator.create(TypeInfo);
        type_info.initDefault();
        return type_info;
    }
};

pub const StructFieldType = struct {
    name: []const u8,
    type_info: *TypeInfo,
    is_public: bool = false,
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
        Map: struct {
            key_type: ?*TypeExpr,
            value_type: *TypeExpr,
            is_mutable: bool,
        },
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
            .Map => |*map| {
                if (map.key_type) |key_type| {
                    key_type.deinit(allocator);
                    allocator.destroy(key_type);
                }
                map.value_type.deinit(allocator);
                allocator.destroy(map.value_type);
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
    is_public: bool = false,
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

fn dumpIndent(writer: anytype, depth: u32) @TypeOf(writer).Error!void {
    for (0..depth) |_| try writer.writeAll("  ");
}

pub fn dumpStatements(writer: anytype, statements: []const Stmt) @TypeOf(writer).Error!void {
    try writer.writeAll("(ast\n");
    for (statements) |*stmt| {
        try dumpStmt(writer, stmt, 1);
    }
    try writer.writeAll(")\n");
}

fn dumpStmt(writer: anytype, stmt: *const Stmt, depth: u32) @TypeOf(writer).Error!void {
    try dumpIndent(writer, depth);
    switch (stmt.data) {
        .Expression => |maybe_expr| {
            try writer.print("Stmt.Expression\n", .{});
            if (maybe_expr) |expr| try dumpExpr(writer, expr, depth + 1);
        },
        .ZigDecl => |z| try writer.print("Stmt.ZigDecl name={s}\n", .{z.name.lexeme}),
        .VarDecl => |*v| {
            try writer.print("Stmt.VarDecl name={s}\n", .{v.name.lexeme});
            if (v.initializer) |init| try dumpExpr(writer, init, depth + 1);
        },
        .Block => |block_stmts| {
            try writer.print("Stmt.Block\n", .{});
            for (block_stmts) |*s| try dumpStmt(writer, s, depth + 1);
        },
        .FunctionDecl => |*f| {
            try writer.print("Stmt.FunctionDecl name={s}\n", .{f.name.lexeme});
            for (f.body) |*s| try dumpStmt(writer, s, depth + 1);
        },
        .MapDecl => |*m| try writer.print("Stmt.MapDecl name={s}\n", .{m.name.lexeme}),
        .Return => |*r| {
            try writer.print("Stmt.Return\n", .{});
            if (r.value) |value| try dumpExpr(writer, value, depth + 1);
        },
        .EnumDecl => |e| try writer.print("Stmt.EnumDecl name={s}\n", .{e.name.lexeme}),
        .MapLiteral => |*ml| {
            try writer.print("Stmt.MapLiteral\n", .{});
            for (ml.entries) |entry| {
                try dumpIndent(writer, depth + 1);
                try writer.print("entry\n", .{});
                try dumpExpr(writer, entry.key, depth + 2);
                try dumpExpr(writer, entry.value, depth + 2);
            }
        },
        .Module => |m| try writer.print("Stmt.Module name={s}\n", .{m.name.lexeme}),
        .Import => try writer.print("Stmt.Import\n", .{}),
        .Path => |p| try writer.print("Stmt.Path {s}\n", .{p}),
        .Continue => try writer.print("Stmt.Continue\n", .{}),
        .Break => try writer.print("Stmt.Break\n", .{}),
        .Assert => |*a| {
            try writer.print("Stmt.Assert\n", .{});
            try dumpExpr(writer, a.condition, depth + 1);
            if (a.message) |msg| try dumpExpr(writer, msg, depth + 1);
        },
        .Cast => |*c| {
            try writer.print("Stmt.Cast\n", .{});
            try dumpExpr(writer, c.value, depth + 1);
            try dumpTypeExpr(writer, c.target_type, depth + 1);
        },
    }
}

fn dumpExpr(writer: anytype, expr: *const Expr, depth: u32) @TypeOf(writer).Error!void {
    try dumpIndent(writer, depth);
    switch (expr.data) {
        .Literal => |lit| {
            try writer.print("Expr.Literal {s}\n", .{@tagName(std.meta.activeTag(lit))});
        },
        .Binary => |b| {
            try writer.print("Expr.Binary op={s}\n", .{b.operator.lexeme});
            if (b.left) |left| try dumpExpr(writer, left, depth + 1);
            if (b.right) |right| try dumpExpr(writer, right, depth + 1);
        },
        .Unary => |u| {
            try writer.print("Expr.Unary op={s}\n", .{u.operator.lexeme});
            if (u.right) |right| try dumpExpr(writer, right, depth + 1);
        },
        .Variable => |t| try writer.print("Expr.Variable {s}\n", .{t.lexeme}),
        .Grouping => |g| {
            try writer.print("Expr.Grouping\n", .{});
            if (g) |e| try dumpExpr(writer, e, depth + 1);
        },
        .Assignment => |a| {
            try writer.print("Expr.Assignment\n", .{});
            if (a.value) |v| try dumpExpr(writer, v, depth + 1);
        },
        .Block => |blk| {
            try writer.print("Expr.Block\n", .{});
            for (blk.statements) |*s| try dumpStmt(writer, s, depth + 1);
            if (blk.value) |v| try dumpExpr(writer, v, depth + 1);
        },
        .Struct => |fields| {
            try writer.print("Expr.Struct\n", .{});
            for (fields) |field| {
                try dumpIndent(writer, depth + 1);
                try writer.print(".{s}\n", .{field.name.lexeme});
                try dumpExpr(writer, field.value, depth + 2);
            }
        },
        .Array => |elements| {
            try writer.print("Expr.Array len={}\n", .{elements.len});
            for (elements) |e| try dumpExpr(writer, e, depth + 1);
        },
        .Index => |i| {
            try writer.print("Expr.Index\n", .{});
            try dumpExpr(writer, i.array, depth + 1);
            try dumpExpr(writer, i.index, depth + 1);
        },
        .FunctionCall => |c| {
            try writer.print("Expr.FunctionCall\n", .{});
            try dumpExpr(writer, c.callee, depth + 1);
            for (c.arguments) |arg| try dumpExpr(writer, arg.expr, depth + 1);
        },
        .If => |i| {
            try writer.print("Expr.If\n", .{});
            if (i.condition) |cond| try dumpExpr(writer, cond, depth + 1);
            if (i.then_branch) |then_| try dumpExpr(writer, then_, depth + 1);
            if (i.else_branch) |else_| try dumpExpr(writer, else_, depth + 1);
        },
        .FieldAccess => |f| {
            try writer.print("Expr.FieldAccess .{s}\n", .{f.field.lexeme});
            try dumpExpr(writer, f.object, depth + 1);
        },
        .StructLiteral => |s| {
            try writer.print("Expr.StructLiteral name={s}\n", .{s.name.lexeme});
            for (s.fields) |f| {
                try dumpIndent(writer, depth + 1);
                try writer.print(".{s}\n", .{f.name.lexeme});
                try dumpExpr(writer, f.value, depth + 2);
            }
        },
        .ReturnExpr => |r| {
            try writer.print("Expr.ReturnExpr\n", .{});
            if (r.value) |v| try dumpExpr(writer, v, depth + 1);
        },
        .TypeExpr => |te| {
            try writer.print("Expr.TypeExpr\n", .{});
            try dumpTypeExpr(writer, te, depth + 1);
        },
        .Loop => |l| {
            try writer.print("Expr.Loop\n", .{});
            if (l.condition) |c| try dumpExpr(writer, c, depth + 1);
            try dumpExpr(writer, l.body, depth + 1);
        },
        .Range => |r| {
            try writer.print("Expr.Range\n", .{});
            try dumpExpr(writer, r.start, depth + 1);
            try dumpExpr(writer, r.end, depth + 1);
        },
        .Peek => |p| {
            try writer.print("Expr.Peek\n", .{});
            try dumpExpr(writer, p.expr, depth + 1);
        },
        .Print => |p| {
            try writer.print("Expr.Print\n", .{});
            if (p.expr) |e| try dumpExpr(writer, e, depth + 1);
        },
        .PeekStruct => |p| {
            try writer.print("Expr.PeekStruct\n", .{});
            try dumpExpr(writer, p.expr, depth + 1);
        },
        .Input => try writer.print("Expr.Input\n", .{}),
        .IndexAssign => |i| {
            try writer.print("Expr.IndexAssign\n", .{});
            try dumpExpr(writer, i.array, depth + 1);
            try dumpExpr(writer, i.index, depth + 1);
            try dumpExpr(writer, i.value, depth + 1);
        },
        .Logical => |l| {
            try writer.print("Expr.Logical op={s}\n", .{l.operator.lexeme});
            try dumpExpr(writer, l.left, depth + 1);
            try dumpExpr(writer, l.right, depth + 1);
        },
        .StructDecl => try writer.print("Expr.StructDecl\n", .{}),
        .FieldAssignment => |f| {
            try writer.print("Expr.FieldAssignment .{s}\n", .{f.field.lexeme});
            try dumpExpr(writer, f.object, depth + 1);
            try dumpExpr(writer, f.value, depth + 1);
        },
        .Exists => |e| {
            try writer.print("Expr.Exists {s}\n", .{e.variable.lexeme});
            try dumpExpr(writer, e.array, depth + 1);
            try dumpExpr(writer, e.condition, depth + 1);
        },
        .ForAll => |e| {
            try writer.print("Expr.ForAll {s}\n", .{e.variable.lexeme});
            try dumpExpr(writer, e.array, depth + 1);
            try dumpExpr(writer, e.condition, depth + 1);
        },
        .ArrayType => |a| {
            try writer.print("Expr.ArrayType\n", .{});
            try dumpTypeExpr(writer, a.element_type, depth + 1);
            if (a.size) |s| try dumpExpr(writer, s, depth + 1);
        },
        .Match => |m| {
            try writer.print("Expr.Match\n", .{});
            try dumpExpr(writer, m.value, depth + 1);
            for (m.cases) |case_| {
                try dumpIndent(writer, depth + 1);
                try writer.print("case\n", .{});
                try dumpExpr(writer, case_.body, depth + 2);
            }
        },
        .EnumDecl => |e| try writer.print("Expr.EnumDecl name={s}\n", .{e.name.lexeme}),
        .EnumMember => |t| try writer.print("Expr.EnumMember {s}\n", .{t.lexeme}),
        .DefaultArgPlaceholder => try writer.print("Expr.DefaultArgPlaceholder\n", .{}),
        .BuiltinCall => |b| {
            try writer.print("Expr.BuiltinCall {s}\n", .{b.function.lexeme});
            for (b.arguments) |arg| try dumpExpr(writer, arg, depth + 1);
        },
        .Map => |m| {
            try writer.print("Expr.Map\n", .{});
            for (m.entries) |entry| {
                try dumpExpr(writer, entry.key, depth + 1);
                try dumpExpr(writer, entry.value, depth + 1);
            }
        },
        .MapLiteral => |m| {
            try writer.print("Expr.MapLiteral\n", .{});
            for (m.entries) |entry| {
                try dumpExpr(writer, entry.key, depth + 1);
                try dumpExpr(writer, entry.value, depth + 1);
            }
            if (m.else_value) |ev| try dumpExpr(writer, ev, depth + 1);
        },
        .InternalCall => |c| {
            try writer.print("Expr.InternalCall .{s}\n", .{c.method.lexeme});
            try dumpExpr(writer, c.receiver, depth + 1);
            for (c.arguments) |arg| try dumpExpr(writer, arg, depth + 1);
        },
        .Increment => |e| {
            try writer.print("Expr.Increment\n", .{});
            try dumpExpr(writer, e, depth + 1);
        },
        .Decrement => |e| {
            try writer.print("Expr.Decrement\n", .{});
            try dumpExpr(writer, e, depth + 1);
        },
        .CompoundAssign => |c| {
            try writer.print("Expr.CompoundAssign name={s} op={s}\n", .{ c.name.lexeme, c.operator.lexeme });
            if (c.value) |value| try dumpExpr(writer, value, depth + 1);
        },
        .Assert => |a| {
            try writer.print("Expr.Assert\n", .{});
            try dumpExpr(writer, a.condition, depth + 1);
            if (a.message) |msg| try dumpExpr(writer, msg, depth + 1);
        },
        .Cast => |c| {
            try writer.print("Expr.Cast\n", .{});
            try dumpExpr(writer, c.value, depth + 1);
            try dumpTypeExpr(writer, c.target_type, depth + 1);
        },
        .Unreachable => try writer.print("Expr.Unreachable\n", .{}),
        .Break => try writer.print("Expr.Break\n", .{}),
        .This => try writer.print("Expr.This\n", .{}),
    }
}

fn dumpTypeExpr(writer: anytype, type_expr: *const TypeExpr, depth: u32) @TypeOf(writer).Error!void {
    try dumpIndent(writer, depth);
    switch (type_expr.data) {
        .Basic => |b| try writer.print("TypeExpr.Basic {s}\n", .{@tagName(b)}),
        .Custom => |t| try writer.print("TypeExpr.Custom {s}\n", .{t.lexeme}),
        .Array => |a| {
            try writer.print("TypeExpr.Array\n", .{});
            try dumpTypeExpr(writer, a.element_type, depth + 1);
            if (a.size) |s| try dumpExpr(writer, s, depth + 1);
        },
        .Struct => |fields| {
            try writer.print("TypeExpr.Struct\n", .{});
            for (fields) |f| {
                try dumpIndent(writer, depth + 1);
                try writer.print("{s}\n", .{f.name.lexeme});
                try dumpTypeExpr(writer, f.type_expr, depth + 2);
            }
        },
        .Enum => |variants| {
            try writer.print("TypeExpr.Enum\n", .{});
            for (variants) |v| try writer.print("  {s}\n", .{v});
        },
        .Union => |types| {
            try writer.print("TypeExpr.Union\n", .{});
            for (types) |t| try dumpTypeExpr(writer, t, depth + 1);
        },
        .Map => |m| {
            try writer.print("TypeExpr.Map\n", .{});
            if (m.key_type) |k| try dumpTypeExpr(writer, k, depth + 1);
            try dumpTypeExpr(writer, m.value_type, depth + 1);
        },
    }
}

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

            const storage_kind: ArrayStorageKind = if (array_size != null) .fixed else .dynamic;

            break :blk TypeInfo{
                .base = .Array,
                .array_type = element_type,
                .array_size = array_size,
                .array_storage = storage_kind,
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
        .Map => |map| blk: {
            const key_type_info = if (map.key_type) |key_type|
                try typeInfoFromExpr(allocator, key_type)
            else
                null;

            const value_type_info = try typeInfoFromExpr(allocator, map.value_type);

            break :blk TypeInfo{
                .base = .Map,
                .map_key_type = key_type_info,
                .map_value_type = value_type_info,
                .is_mutable = map.is_mutable,
            };
        },
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

        .Union => |union_info| {
            const members = union_info.members;
            var member_types = try allocator.alloc(*TypeInfo, members.len);
            for (members, 0..) |member_ptr, i| {
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
