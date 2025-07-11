const std = @import("std");
const Token = @import("../types/token.zig").Token;
const TokenLiteral = @import("../types/types.zig").TokenLiteral;
const Reporting = @import("../utils/reporting.zig");
const HIRType = @import("../codegen/hir/soxa_types.zig").HIRType;

/// Counter for generating unique node IDs
var next_node_id: NodeId = 0;

/// Unique identifier for AST nodes to help with HIR mapping and error tracking
pub const NodeId = u32;

/// Generate a new unique node ID
pub fn generateNodeId() NodeId {
    defer next_node_id += 1;
    return next_node_id;
}

/// Tracks the exact source code location of an AST node
pub const SourceSpan = struct {
    start: Reporting.Reporter.Location,
    end: Reporting.Reporter.Location,

    pub fn fromToken(token: Token) SourceSpan {
        return .{
            .start = .{
                .file = "", // Default to empty string since Token doesn't have file info
                .line = token.line,
                .column = token.column,
            },
            .end = .{
                .file = "", // Default to empty string since Token doesn't have file info
                .line = token.line,
                .column = token.column + token.lexeme.len,
            },
        };
    }

    pub fn merge(start: SourceSpan, end: SourceSpan) SourceSpan {
        return .{
            .start = start.start,
            .end = end.end,
        };
    }
};

//======================================================================
// Core Types
//======================================================================

// TODO: location in Reporting too, why?
pub const Location = struct {
    file: []const u8,
    line: i32,
    column: usize,
};

//======================================================================
// Struct Types
//======================================================================

pub const StructField = struct {
    name: Token,
    type_expr: *TypeExpr,

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

pub const StructDecl = struct {
    name: Token,
    fields: []*StructField,
    is_public: bool = false,
};

//======================================================================
// Function Types
//======================================================================

pub const FunctionParam = struct {
    name: Token,
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
};

//======================================================================
// Visitor Pattern
//======================================================================

/// Base interface for AST visitors
pub const ASTVisitor = struct {
    /// Visit an expression node
    visitExpr: fn (self: *ASTVisitor, expr: *Expr) anyerror!void,
    /// Visit a statement node
    visitStmt: fn (self: *ASTVisitor, stmt: *Stmt) anyerror!void,
    /// Visit a type expression
    visitTypeExpr: fn (self: *ASTVisitor, type_expr: *TypeExpr) anyerror!void,

    /// Optional hooks for enter/exit of compound nodes
    enterScope: ?fn (self: *ASTVisitor) anyerror!void = null,
    exitScope: ?fn (self: *ASTVisitor) anyerror!void = null,
};

/// Common base for all AST nodes
pub const Base = struct {
    id: NodeId,
    span: SourceSpan,
};

//======================================================================
// Statement Types
//======================================================================

pub const TryStmt = struct {
    try_body: []Stmt,
    catch_body: []Stmt,
    error_var: ?Token,
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
        Return: struct {
            value: ?*Expr,
            type_info: TypeInfo,
        },
        EnumDecl: EnumDecl,
        Map: []MapEntry,
        Try: TryStmt,
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
            location: Reporting.Reporter.Location,
            message: ?*Expr = null,
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
        }
    }
};

//======================================================================
// Expression Types
//======================================================================

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

pub const CompoundAssignment = struct {
    name: Token,
    operator: Token,
    value: ?*Expr,
};

pub const MapEntry = struct {
    key: *Expr,
    value: *Expr,
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
    item_name: Token,
    array: *Expr,
    body: []Stmt,
};

pub const PeekExpr = struct {
    expr: *Expr,
    location: Reporting.Reporter.Location,
    variable_name: ?[]const u8,
    field_name: ?[]const u8 = null,
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
        PeekStruct: struct {
            expr: *Expr,
            location: Reporting.Reporter.Location,
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
            call_context: ?FunctionCallRef = null,
        },
        Logical: Logical,
        FunctionExpr: struct {
            name: Token,
            params: []FunctionParam,
            return_type_info: TypeInfo,
            body: []Stmt,
            is_entry: bool = false,
            is_public: bool = false,
            defining_module: ?[]const u8 = null,
        },
        While: WhileExpr,
        For: ForExpr,
        ForEach: ForEachExpr,
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
        TypeOf: *Expr,
        LengthOf: *Expr,
        BytesOf: *Expr,
        Map: []MapEntry,
        MethodCall: struct {
            receiver: *Expr,
            method: Token,
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
        Assert: struct {
            condition: *Expr,
            location: Reporting.Reporter.Location,
            message: ?*Expr = null,
        },
        ReturnExpr: struct {
            value: ?*Expr,
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
                    .byte => {}, // No cleanup needed for byte
                    else => {}, // Other literals don't own memory
                }
            },
            .Logical => |*l| {
                l.left.deinit(allocator);
                allocator.destroy(l.left);
                l.right.deinit(allocator);
                allocator.destroy(l.right);
            },
            .FunctionExpr => |*f| {
                for (f.params) |*param| {
                    param.deinit(allocator);
                }
                allocator.free(f.params);
                for (f.body) |*stmt| {
                    stmt.deinit(allocator);
                }
                allocator.free(f.body);
            },
            .Peek => |i| {
                i.expr.deinit(allocator);
                allocator.destroy(i.expr);
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
            .LengthOf => |expr| {
                expr.deinit(allocator);
                allocator.destroy(expr);
            },
            .BytesOf => |expr| {
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
            .PeekStruct => |peek| {
                peek.expr.deinit(allocator);
                allocator.destroy(peek.expr);
            },
        }
    }
};

//======================================================================
// Type System
//======================================================================

pub const Type = enum {
    Int,
    Byte,
    Float,
    String,
    Tetra,
    Array,
    Tuple,
    Function,
    Struct,
    Enum,
    Custom,
    Map,
    Nothing,
    Reference,
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
    referenced_type: ?*TypeInfo = null,
    union_type: ?*UnionType = null,

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
            .tuple => .Tuple,
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
    current_type_index: ?u32 = null, // Index into types array

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
            else => {},
        }
    }
};

//======================================================================
// Module System
//======================================================================

pub const ImportInfo = struct {
    module_path: []const u8,
    namespace_alias: ?[]const u8 = null,
    specific_symbol: ?[]const u8 = null,
};

pub const ModuleSymbol = struct {
    name: []const u8,
    kind: enum { Function, Variable, Struct, Enum },
    is_public: bool,
    stmt_index: usize, // Index in the module's statements for quick lookup
};

pub const ModuleInfo = struct {
    name: []const u8,
    imports: []const ImportInfo,
    ast: ?*Expr = null, // Store the module's AST for reference
    file_path: []const u8, // Store the file path for detecting self-imports
    symbols: ?std.StringHashMap(ModuleSymbol) = null, // All symbols defined in this module

    // Function to check if a symbol exists and is public
    pub fn hasPublicSymbol(self: *const ModuleInfo, symbol_name: []const u8) bool {
        if (self.symbols) |symbols| {
            if (symbols.get(symbol_name)) |symbol| {
                return symbol.is_public;
            }
        }
        return false;
    }

    // Function to check if a symbol exists (regardless of visibility)
    pub fn hasSymbol(self: *const ModuleInfo, symbol_name: []const u8) bool {
        if (self.symbols) |symbols| {
            return symbols.contains(symbol_name);
        }
        return false;
    }

    // Get a symbol by name
    pub fn getSymbol(self: *const ModuleInfo, symbol_name: []const u8) ?ModuleSymbol {
        if (self.symbols) |symbols| {
            return symbols.get(symbol_name);
        }
        return null;
    }

    // Deinit the ModuleInfo
    pub fn deinit(self: *ModuleInfo, allocator: std.mem.Allocator) void {
        allocator.free(self.imports);
        if (self.symbols) |*symbols| {
            symbols.deinit();
        }
    }
};

//======================================================================
// Node Creation Utilities
//======================================================================

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

//======================================================================
// Type System Utilities
//======================================================================

// Helper function to create TypeInfo from type expression
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

            // Extract array size if present
            var array_size: ?usize = null;
            if (array.size) |size_expr| {
                if (size_expr.data == .Literal) {
                    switch (size_expr.data.Literal) {
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
        .Custom => |custom_token| TypeInfo{ .base = .Custom, .custom_type = custom_token.lexeme },
        .Enum => TypeInfo{ .base = .Nothing },
    };

    return type_info;
}

//======================================================================
// Semantic Context Structures (for HIR generation)
//======================================================================

/// Represents the resolution context of a variable reference
pub const VariableRef = struct {
    token: Token, // Original token (for error reporting)
    module_context: ?[]const u8 = null, // Which module this variable belongs to ("math", "utils", etc.)
    scope_depth: u32 = 0, // Local scope depth (0 = global, 1 = function, 2 = nested block)
    resolution_kind: ResolutionKind,

    pub const ResolutionKind = enum {
        Local, // Local variable in current function
        ModuleGlobal, // Global variable in current module
        ImportedModule, // Imported module namespace (e.g., "math" in "math.add()")
        ImportedSymbol, // Imported symbol from another module
        Unresolved, // Not yet resolved (will be resolved in semantic analysis)
    };
};

/// Represents the resolution context of a function call
pub const FunctionCallRef = struct {
    name: []const u8,
    call_kind: CallKind,
    target_module: ?[]const u8 = null, // For cross-module calls

    pub const CallKind = enum {
        LocalFunction, // Function in current module
        ModuleFunction, // Function in imported module (namespace.func)
        BuiltinFunction, // Built-in function (print, length, etc.)
        Unresolved, // Not yet resolved
    };
};

//======================================================================
// AST Node Creation Helpers (for backwards compatibility)
//======================================================================

/// Create a variable reference with basic token (defaults to Unresolved)
pub fn createVariableRef(token: Token) VariableRef {
    return VariableRef{
        .token = token,
        .resolution_kind = .Unresolved,
    };
}

/// Create a resolved variable reference with full context
pub fn createResolvedVariableRef(token: Token, module_context: ?[]const u8, scope_depth: u32, kind: VariableRef.ResolutionKind) VariableRef {
    return VariableRef{
        .token = token,
        .module_context = module_context,
        .scope_depth = scope_depth,
        .resolution_kind = kind,
    };
}

/// Create a function call context
pub fn createFunctionCallRef(name: []const u8, kind: FunctionCallRef.CallKind, target_module: ?[]const u8) FunctionCallRef {
    return FunctionCallRef{
        .name = name,
        .call_kind = kind,
        .target_module = target_module,
    };
}

//======================================================================
// Stack-based HIR Instructions (for reference)
//======================================================================

/// HIR Instruction set for stack-based virtual machine
/// This shows how the enhanced AST structures map to HIR instructions
///
/// Example HIR generation mapping:
///
/// AST: Variable{.token = "x", .resolution_kind = .Local, .scope_depth = 1}
/// HIR: LoadLocal(stack_offset)
///
/// AST: Variable{.token = "math", .resolution_kind = .ImportedModule}
/// HIR: (namespace marker - resolved during field access)
///
/// AST: Call{.callee = FieldAccess{.object = "math", .field = "add"}, .call_context = .ModuleFunction}
/// HIR: CallModule{.module = "math", .function = "add", .arg_count = 2}
pub const HIRInstruction = union(enum) {
    // Stack operations
    Push: TokenLiteral, // Push literal value
    Pop, // Pop top value

    // Variable operations
    LoadLocal: u32, // Load local variable by stack offset
    StoreLocal: u32, // Store to local variable by stack offset
    LoadGlobal: []const u8, // Load module global by name
    StoreGlobal: []const u8, // Store to module global by name
    LoadModule: struct { // Load from imported module
        module: []const u8,
        symbol: []const u8,
    },

    // Arithmetic operations
    Add,
    Sub,
    Mul,
    Div,
    Mod, // Binary arithmetic
    Neg,
    Not, // Unary operations

    // Comparison operations
    Eq,
    Ne,
    Lt,
    Le,
    Gt,
    Ge, // Comparisons

    // Control flow
    Jmp: []const u8, // Unconditional jump to label
    JmpTrue: []const u8, // Jump if top of stack is true
    JmpFalse: []const u8, // Jump if top of stack is false
    Label: []const u8, // Label for jumps

    // Function operations
    CallLocal: struct { // Call function in current module
        name: []const u8,
        arg_count: u32,
    },
    CallModule: struct { // Call function in imported module
        module: []const u8,
        function: []const u8,
        arg_count: u32,
    },
    CallBuiltin: struct { // Call built-in function
        name: []const u8,
        arg_count: u32,
    },
    Return, // Return from function

    // Debug operations
    Peek: ?[]const u8, // Print value with optional name

    // Array operations
    MakeArray: u32, // Create array from top N stack values
    IndexLoad, // Load array[index]
    IndexStore, // Store to array[index]

    // Block operations
    EnterScope, // Enter new scope
    ExitScope, // Exit scope
};
