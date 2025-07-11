const std = @import("std");
const ast = @import("../ast/ast.zig");
const Memory = @import("../utils/memory.zig");
const MemoryManager = Memory.MemoryManager;
const Scope = Memory.Scope;
const ScopeManager = Memory.ScopeManager;
const Variable = Memory.Variable;

const Reporting = @import("../utils/reporting.zig");
const Reporter = Reporting.Reporter;
const ErrorList = Reporting.ErrorList;

const Types = @import("../types/types.zig");
const TokenLiteral = Types.TokenLiteral;

const TokenImport = @import("../types/token.zig");
const TokenType = TokenImport.TokenType;

//======================================================================

const NodeId = u32; // Or whatever your AST uses

pub const SemanticAnalyzer = struct {
    allocator: std.mem.Allocator,
    reporter: *Reporter,
    memory: *MemoryManager,
    fatal_error: bool,
    current_scope: ?*Scope,
    type_cache: std.AutoHashMap(NodeId, *ast.TypeInfo),

    pub fn init(allocator: std.mem.Allocator, reporter: *Reporter, memory: *MemoryManager) SemanticAnalyzer {
        return .{
            .allocator = allocator,
            .reporter = reporter,
            .memory = memory,
            .fatal_error = false,
            .current_scope = null,
            .type_cache = std.AutoHashMap(NodeId, *ast.TypeInfo).init(allocator),
        };
    }

    pub fn deinit(self: *SemanticAnalyzer) void {
        var it = self.type_cache.valueIterator();
        while (it.next()) |type_info| {
            type_info.*.deinit(self.allocator);
            self.allocator.destroy(type_info.*);
        }
        self.type_cache.deinit();
    }

    pub fn analyze(self: *SemanticAnalyzer, statements: []ast.Stmt) ErrorList!void {
        const root_scope = try self.memory.scope_manager.createScope(null);
        self.memory.scope_manager.root_scope = root_scope;
        self.current_scope = root_scope;

        try self.collectDeclarations(statements, root_scope);

        if (self.fatal_error) {
            return error.SemanticError;
        }

        try self.validateStatements(statements);
    }

    fn collectDeclarations(self: *SemanticAnalyzer, statements: []ast.Stmt, scope: *Scope) ErrorList!void {
        for (statements) |stmt| {
            switch (stmt.data) {
                .VarDecl => |decl| {
                    // Create TypeInfo
                    const type_info = try self.allocator.create(ast.TypeInfo);
                    errdefer self.allocator.destroy(type_info);

                    if (decl.type_info.base != .Nothing) {
                        // Use explicit type
                        type_info.* = decl.type_info;
                    } else if (decl.initializer) |init_expr| {
                        // Infer from initializer
                        const inferred = try self.inferTypeFromExpr(init_expr);
                        type_info.* = inferred.*;
                    } else {
                        self.reporter.reportCompileError(
                            stmt.base.span.start,
                            "Variable declaration requires either type annotation or initializer",
                            .{},
                        );
                        self.fatal_error = true;
                        continue;
                    }

                    // Convert TypeInfo to TokenType
                    const token_type = self.convertTypeToTokenType(type_info.base);

                    // Create default value based on type
                    const default_value = switch (type_info.base) {
                        .Int => TokenLiteral{ .int = 0 },
                        .Float => TokenLiteral{ .float = 0.0 },
                        .String => TokenLiteral{ .string = "" },
                        .Tetra => TokenLiteral{ .tetra = .false },
                        .Byte => TokenLiteral{ .byte = 0 },
                        else => TokenLiteral{ .nothing = {} },
                    };

                    // Add to scope
                    _ = try scope.createValueBinding(
                        decl.name.lexeme,
                        default_value,
                        token_type,
                        type_info.*,
                        !type_info.is_mutable,
                    );
                },
                .Block => |block_stmts| {
                    const block_scope = try self.memory.scope_manager.createScope(scope);
                    try self.collectDeclarations(block_stmts, block_scope);
                },
                // TODO: Handle other declarations...
                else => {},
            }
        }
    }

    fn convertTypeToTokenType(self: *SemanticAnalyzer, base_type: ast.Type) TokenType {
        _ = self;
        return switch (base_type) {
            .Int => .INT,
            .Float => .FLOAT,
            .String => .STRING,
            .Tetra => .TETRA,
            .Byte => .BYTE,
            .Array => .ARRAY,
            .Function => .FUNCTION,
            .Struct => .STRUCT,
            .Nothing => .NOTHING,
            .Tuple => .TUPLE,
            .Map => .MAP,
            .Custom => .CUSTOM,
            .Reference => .REFERENCE,
            .Enum => .ENUM,
            .Union => .UNION,
        };
    }

    fn validateStatements(self: *SemanticAnalyzer, statements: []ast.Stmt) ErrorList!void {
        for (statements) |stmt| {
            switch (stmt.data) {
                .VarDecl => |decl| {
                    if (decl.initializer) |init_expr| {
                        const init_type = try self.inferTypeFromExpr(init_expr);
                        if (decl.type_info.base != .Nothing) {
                            // Check initializer matches declared type
                            // Create a mutable copy for unifyTypes
                            var decl_type_copy = decl.type_info;
                            try self.unifyTypes(&decl_type_copy, init_type, stmt.base.span);
                        }
                    }
                },
                .Block => |block_stmts| {
                    const prev_scope = self.current_scope;
                    self.current_scope = try self.memory.scope_manager.createScope(self.current_scope);
                    defer {
                        self.current_scope = prev_scope;
                        // Scope cleanup handled by memory manager
                    }
                    try self.validateStatements(block_stmts);
                },
                // TODO: Handle other statements...
                else => {},
            }
        }
    }

    fn unifyTypes(self: *SemanticAnalyzer, expected: *ast.TypeInfo, actual: *ast.TypeInfo, span: ast.SourceSpan) !void {
        if (expected.base != actual.base) {
            // Special cases (like int to float conversion)
            if (expected.base == .Float and actual.base == .Int) {
                return; // Allow implicit int to float
            }

            self.reporter.reportCompileError(
                span.start,
                "Type mismatch: expected {s}, got {s}",
                .{ @tagName(expected.base), @tagName(actual.base) },
            );
            self.fatal_error = true;
        }

        // Handle complex type unification (arrays, structs, etc)
        switch (expected.base) {
            .Array => {
                if (expected.array_type) |exp_elem| {
                    if (actual.array_type) |act_elem| {
                        try self.unifyTypes(exp_elem, act_elem, span);
                    }
                }
            },
            // Handle other complex types...
            else => {},
        }
    }

    fn inferTypeFromExpr(self: *SemanticAnalyzer, expr: *ast.Expr) !*ast.TypeInfo {
        // Check cache first
        if (self.type_cache.get(expr.base.id)) |cached| {
            return cached;
        }

        var type_info = try self.allocator.create(ast.TypeInfo);
        errdefer self.allocator.destroy(type_info);

        switch (expr.data) {
            .Literal => |lit| {
                type_info.inferFrom(lit); // TokenLiteral is already the right type
            },
            .Binary => |bin| {
                const left_type = try self.inferTypeFromExpr(bin.left.?);
                const right_type = try self.inferTypeFromExpr(bin.right.?);
                try self.unifyTypes(left_type, right_type, expr.base.span);
                type_info.* = left_type.*;
            },
            .Variable => |var_token| {
                // Look up in current and parent scopes
                if (self.lookupVariable(var_token.lexeme)) |variable| {
                    if (self.memory.scope_manager.value_storage.get(variable.storage_id)) |storage| {
                        type_info.* = storage.type_info;
                    } else {
                        self.reporter.reportCompileError(
                            expr.base.span.start,
                            "Internal error: Variable storage not found",
                            .{},
                        );
                        self.fatal_error = true;
                        type_info.base = .Nothing;
                    }
                } else {
                    self.reporter.reportCompileError(
                        expr.base.span.start,
                        "Undefined variable",
                        .{},
                    );
                    self.fatal_error = true;
                    type_info.base = .Nothing;
                }
            },
            // TODO: Handle other expression types...
            else => {},
        }

        // Cache the result
        try self.type_cache.put(expr.base.id, type_info);
        return type_info;
    }

    fn lookupVariable(self: *SemanticAnalyzer, name: []const u8) ?*Variable {
        if (self.current_scope) |scope| {
            return scope.lookupVariable(name);
        }
        return null;
    }
};
