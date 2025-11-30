const std = @import("std");
const token = @import("../types/token.zig");
const declaration_parser = @import("declaration_parser.zig");
const expression_parser = @import("expression_parser.zig");
const statement_parser = @import("statement_parser.zig");
const Precedence = @import("./precedence.zig").Precedence;
const precedence = @import("./precedence.zig");
const LexicalAnalyzer = @import("../analysis/lexical.zig").LexicalAnalyzer;
const import_parser = @import("import_parser.zig");
const module_resolver = @import("module_resolver.zig");

const ast = @import("../ast/ast.zig");
const ModuleInfo = ast.ModuleInfo;

const Reporting = @import("../utils/reporting.zig");
const Reporter = Reporting.Reporter;
const Location = Reporting.Location;

const Errors = @import("../utils/errors.zig");
const ErrorList = Errors.ErrorList;
const ErrorCode = Errors.ErrorCode;

const TokenStyle = enum {
    Keyword,
    Symbol,
    Undefined,
};

pub const ModuleResolutionStatus = enum {
    NOT_STARTED,
    IN_PROGRESS,
    COMPLETED,
};

pub const ImportStackEntry = struct {
    module_path: []const u8,
    imported_from: ?[]const u8,

    pub fn format(self: ImportStackEntry, comptime fmt: []const u8, options: std.fmt.FormatOptions, writer: anytype) !void {
        _ = fmt;
        _ = options;
        if (self.imported_from) |from| {
            try writer.print("{s} (imported from {s})", .{ self.module_path, from });
        } else {
            try writer.print("{s}", .{self.module_path});
        }
    }
};

pub const Parser = struct {
    tokens: []const token.Token,
    current: usize,
    allocator: std.mem.Allocator,
    reporter: *Reporter,

    has_entry_point: bool = false,
    entry_point_location: ?token.Token = null,
    entry_point_name: ?[]const u8 = null,

    current_file: []const u8,
    current_file_uri: []const u8,
    current_module: ?ModuleInfo = null,
    module_cache: std.StringHashMap(ModuleInfo),
    module_namespaces: std.StringHashMap(ModuleInfo),

    module_imports: std.StringHashMap(std.StringHashMap([]const u8)),

    imported_symbols: ?std.StringHashMap(import_parser.ImportedSymbol) = null,

    declared_types: std.StringHashMap(void),

    module_resolution_status: std.StringHashMap(ModuleResolutionStatus),
    import_stack: std.array_list.Managed(ImportStackEntry),

    pub fn init(allocator: std.mem.Allocator, tokens: []const token.Token, current_file: []const u8, current_file_uri: []const u8, reporter: *Reporter) Parser {
        const parser = Parser{
            .allocator = allocator,
            .tokens = tokens,
            .current = 0,
            .reporter = reporter,
            .current_file = current_file,
            .current_file_uri = current_file_uri,
            .module_cache = std.StringHashMap(ModuleInfo).init(allocator),
            .module_namespaces = std.StringHashMap(ModuleInfo).init(allocator),
            .module_imports = std.StringHashMap(std.StringHashMap([]const u8)).init(allocator),
            .imported_symbols = std.StringHashMap(import_parser.ImportedSymbol).init(allocator),
            .declared_types = std.StringHashMap(void).init(allocator),
            .module_resolution_status = std.StringHashMap(ModuleResolutionStatus).init(allocator),
            .import_stack = std.array_list.Managed(ImportStackEntry).init(allocator),
        };

        return parser;
    }

    pub fn deinit(self: *Parser) void {
        self.module_cache.deinit();
        self.module_namespaces.deinit();
        self.module_imports.deinit();
        if (self.imported_symbols) |*imported_symbols| {
            imported_symbols.deinit();
        }
        self.declared_types.deinit();
        self.module_resolution_status.deinit();
        self.import_stack.deinit();
    }

    pub fn peek(self: *Parser) token.Token {
        var i = self.current;
        while (i < self.tokens.len and self.tokens[i].type == .SEMICOLON) {
            i += 1;
        }
        if (i >= self.tokens.len) {
            return self.tokens[self.tokens.len - 1];
        }
        return self.tokens[i];
    }

    pub fn peekAhead(self: *Parser, offset: usize) token.Token {
        var i: usize = self.current;
        var remaining = offset;
        while (i < self.tokens.len and self.tokens[i].type == .SEMICOLON) {
            i += 1;
        }
        while (remaining > 0 and i < self.tokens.len) {
            i += 1;
            if (i >= self.tokens.len) break;
            if (self.tokens[i].type == .SEMICOLON) continue;
            remaining -= 1;
        }
        if (i >= self.tokens.len) {
            return self.tokens[self.tokens.len - 1];
        }
        while (i < self.tokens.len and self.tokens[i].type == .SEMICOLON) {
            i += 1;
        }
        if (i >= self.tokens.len) {
            return self.tokens[self.tokens.len - 1];
        }
        return self.tokens[i];
    }

    pub fn advance(self: *Parser) void {
        if (self.current < self.tokens.len - 1) {
            self.current += 1;
            while (self.current < self.tokens.len - 1 and self.tokens[self.current].type == .SEMICOLON) {
                self.current += 1;
            }
        }
    }

    pub fn block(self: *Parser, _: ?*ast.Expr, _: Precedence) ErrorList!?*ast.Expr {
        if (self.peek().type != .LEFT_BRACE) {
            return error.ExpectedLeftBrace;
        }
        self.advance();

        var statements = std.array_list.Managed(ast.Stmt).init(self.allocator);
        errdefer {
            for (statements.items) |*stmt| {
                stmt.deinit(self.allocator);
            }
            statements.deinit();
        }

        var last_expr: ?*ast.Expr = null;

        while (self.peek().type != .RIGHT_BRACE and self.peek().type != .EOF) {
            const stmt = try statement_parser.parseStatement(self);
            try statements.append(stmt);

            if (stmt.data == .Return) break;

            if (self.peek().type == .RIGHT_BRACE) {
                break;
            }
        }

        if (self.peek().type != .RIGHT_BRACE) {
            return error.ExpectedRightBrace;
        }
        self.advance();

        if (statements.items.len > 0) {
            const last_idx = statements.items.len - 1;
            const last_stmt = statements.items[last_idx];
            switch (last_stmt.data) {
                .Expression => |maybe_expr| {
                    if (maybe_expr) |e| {
                        last_expr = e;
                    }
                    statements.items.len = last_idx;
                },
                else => {},
            }
        }

        const block_expr = try self.allocator.create(ast.Expr);
        block_expr.* = .{
            .base = .{
                .id = ast.generateNodeId(),
                .span = ast.SourceSpan.fromToken(self.peek()),
            },
            .data = .{
                .Block = .{
                    .statements = try statements.toOwnedSlice(),
                    .value = last_expr,
                },
            },
        };

        return block_expr;
    }

    pub fn execute(self: *Parser) ErrorList![]ast.Stmt {
        var statements = std.array_list.Managed(ast.Stmt).init(self.allocator);
        errdefer {
            for (statements.items) |*stmt| {
                stmt.deinit(self.allocator);
            }
            statements.deinit();
        }

        // first pass: imports and modules
        var start_index_after_imports = self.current;
        while (self.tokens[start_index_after_imports].type == .IMPORT or self.tokens[start_index_after_imports].type == .MODULE) {
            const temp_current = self.current;
            self.current = start_index_after_imports;

            if (self.tokens[start_index_after_imports].type == .MODULE) {
                _ = try import_parser.parseModuleStmt(self);
            } else {
                _ = try import_parser.parseImportStmt(self);
            }

            start_index_after_imports = self.current;

            self.current = temp_current;
        }
        self.current = start_index_after_imports;

        // second pass: all other statements
        while (self.peek().type != .EOF) {
            const loop_start_pos = self.current;

            var is_public = false;
            var is_entry = false;

            if (self.peek().type == .ENTRY) {
                is_entry = true;
                const entry_token = self.peek();
                self.advance();
                if (!self.has_entry_point) {
                    self.has_entry_point = true;
                    self.entry_point_location = entry_token;
                }
            }
            if (self.peek().type == .PUBLIC) {
                is_public = true;
                self.advance();
            }

            const stmt_token_type = self.peek().type;
            switch (stmt_token_type) {
                .VAR, .CONST => {
                    var decl = try declaration_parser.parseVarDecl(self);
                    decl.data.VarDecl.is_public = is_public;
                    if (is_entry) {
                        return error.InvalidEntryPoint;
                    }
                    try statements.append(decl);
                },
                .MAP_TYPE => {
                    const map_stmt = try declaration_parser.parseMapDecl(self, is_public);
                    if (is_entry) {
                        return error.InvalidEntryPoint;
                    }
                    try statements.append(map_stmt);
                },
                .FUNCTION => {
                    var func = try declaration_parser.parseFunctionDecl(self);
                    func.data.FunctionDecl.is_public = is_public;
                    func.data.FunctionDecl.is_entry = is_entry;
                    if (is_entry) {
                        if (self.entry_point_location != null) {
                            self.entry_point_name = func.data.FunctionDecl.name.lexeme;
                        } else {
                            {
                                return error.MultipleEntryPoints;
                            }
                        }
                    }
                    try statements.append(func);
                },
                .IMPORT => {
                    if (is_entry) {
                        return error.MisplacedEntryPoint;
                    }
                    if (is_public) {
                        return error.MisplacedPublicModifier;
                    }
                    _ = try import_parser.parseImportStmt(self);
                },
                .MODULE => {
                    if (is_entry) {
                        return error.MisplacedEntryPoint;
                    }
                    if (is_public) {
                        return error.MisplacedPublicModifier;
                    }
                    _ = try import_parser.parseModuleStmt(self);
                },
                .STRUCT_TYPE => {
                    const expr = try declaration_parser.parseStructDecl(self, null, .NONE);
                    if (expr) |non_null_expr| {
                        switch (non_null_expr.data) {
                            .StructDecl => |*struct_decl| {
                                struct_decl.is_public = is_public;
                            },
                            else => {},
                        }
                    }
                    if (is_entry) {
                        return error.InvalidEntryPoint;
                    }
                    if (expr) |e| {
                        try statements.append(.{
                            .base = .{
                                .id = ast.generateNodeId(),
                                .span = ast.SourceSpan.fromToken(self.peek()),
                            },
                            .data = .{ .Expression = e },
                        });
                    }
                },
                .ENUM_TYPE => {
                    var enum_decl = try declaration_parser.parseEnumDecl(self);
                    enum_decl.data.EnumDecl.is_public = is_public;
                    if (is_entry) {
                        return error.InvalidEntryPoint;
                    }
                    try statements.append(enum_decl);
                },
                .IF, .WHILE, .RETURN, .LEFT_BRACE, .EACH => {
                    if (is_entry) {
                        return error.MisplacedEntryPoint;
                    }
                    if (is_public) {
                        return error.MisplacedPublicModifier;
                    }
                    const parsed_stmt = try statement_parser.parseStatement(self);
                    if (!(parsed_stmt.data == .Expression and parsed_stmt.data.Expression == null)) {
                        try statements.append(parsed_stmt);
                    }
                },
                .ASSERT => {
                    if (is_entry) {
                        return error.MisplacedEntryPoint;
                    }
                    if (is_public) {
                        return error.MisplacedPublicModifier;
                    }
                    const parsed_stmt = try statement_parser.parseStatement(self);
                    if (!(parsed_stmt.data == .Expression and parsed_stmt.data.Expression == null)) {
                        try statements.append(parsed_stmt);
                    }
                },
                else => {
                    if (is_entry) {
                        return error.MisplacedEntryPoint;
                    }
                    if (is_public) {
                        return error.MisplacedPublicModifier;
                    }
                    const expr_stmt = try statement_parser.parseExpressionStmt(self);
                    if (!(expr_stmt.data == .Expression and expr_stmt.data.Expression == null)) {
                        try statements.append(expr_stmt);
                    }
                },
            }

            if (self.current == loop_start_pos and self.peek().type != .EOF) {
                self.reporter.reportCompileError(Location{
                    .file = self.current_file,
                    .file_uri = self.current_file_uri,
                    .range = .{
                        .start_line = self.peek().line,
                        .start_col = self.peek().column,
                        .end_line = self.peek().line,
                        .end_col = self.peek().column,
                    },
                }, ErrorCode.PARSER_DID_NOT_ADVANCE, "Parser did not advance", .{});
                return error.ParserDidNotAdvance;
            }
        }

        if (self.has_entry_point and self.entry_point_name == null) {
            const loc = Location{
                .file = self.current_file,
                .file_uri = self.current_file_uri,
                .range = .{
                    .start_line = self.entry_point_location.?.line,
                    .start_col = self.entry_point_location.?.column,
                    .end_line = self.entry_point_location.?.line,
                    .end_col = self.entry_point_location.?.column,
                },
            };
            self.reporter.reportCompileError(loc, ErrorCode.MISSING_ENTRY_POINT_FUNCTION, "Entry point marker '->' not followed by a function declaration", .{});
            return error.MissingEntryPointFunction;
        }

        return statements.toOwnedSlice();
    }

    pub fn hasReturnWithValue(self: *Parser) !bool {
        var pos = self.current;
        while (pos < self.tokens.len and self.tokens[pos].type != .LEFT_BRACE) {
            pos += 1;
        }
        if (pos >= self.tokens.len) return false;

        pos += 1;
        var brace_count: usize = 1;
        var found_return_value = false;

        while (pos < self.tokens.len) {
            const current_token = self.tokens[pos];

            switch (current_token.type) {
                .LEFT_BRACE => {
                    brace_count += 1;
                },
                .RIGHT_BRACE => {
                    brace_count -= 1;
                    if (brace_count == 0) break;
                },
                .RETURN => {
                    if (pos + 1 < self.tokens.len) {
                        const next_token = self.tokens[pos + 1];
                        if (next_token.type != .NEWLINE) {
                            found_return_value = true;
                            break;
                        }
                    }
                },
                else => {},
            }

            pos += 1;
        }

        return found_return_value;
    }

    pub fn call(self: *Parser, callee: ?*ast.Expr, _: Precedence) ErrorList!?*ast.Expr {
        self.advance();

        var arguments = std.array_list.Managed(ast.CallArgument).init(self.allocator);
        errdefer {
            for (arguments.items) |arg| {
                arg.expr.deinit(self.allocator);
                self.allocator.destroy(arg.expr);
            }
            arguments.deinit();
        }

        if (self.peek().type != .RIGHT_PAREN) {
            while (true) {
                var arg_expr: *ast.Expr = undefined;
                var is_alias = false;
                if (self.peek().type == .TILDE) {
                    self.advance();
                    const placeholder = try self.allocator.create(ast.Expr);
                    placeholder.* = .{
                        .base = .{
                            .id = ast.generateNodeId(),
                            .span = ast.SourceSpan.fromToken(self.peek()),
                        },
                        .data = .DefaultArgPlaceholder,
                    };
                    arg_expr = placeholder;
                } else if (self.peek().type == .CARET) {
                    self.advance();
                    arg_expr = try expression_parser.parseExpression(self) orelse return error.ExpectedExpression;
                    is_alias = true;
                } else {
                    arg_expr = try expression_parser.parseExpression(self) orelse return error.ExpectedExpression;
                }
                try arguments.append(.{ .expr = arg_expr, .is_alias = is_alias });

                if (self.peek().type == .RIGHT_PAREN) break;
                if (self.peek().type != .COMMA) return error.ExpectedComma;
                self.advance();
            }
        }
        if (self.peek().type != .RIGHT_PAREN) {
            return error.ExpectedRightParen;
        }
        self.advance();
        if (callee.?.data == .FieldAccess) {
            const fa = callee.?.data.FieldAccess;
            if (Parser.methodNameToTokenType(fa.field.lexeme)) |_| {
                const loc: Location = .{ .file = self.current_file, .file_uri = self.current_file_uri, .range = .{ .start_line = fa.field.line, .start_col = fa.field.column, .end_line = fa.field.line, .end_col = fa.field.column } };
                self.reporter.reportCompileError(loc, ErrorCode.UNKNOWN_METHOD, "Unknown field or method '{s}'. If this is a compiler method, use @{s}(...)", .{ fa.field.lexeme, fa.field.lexeme });
                return error.UnknownFieldOrMethod;
            }
        }

        const call_expr = try self.allocator.create(ast.Expr);
        call_expr.* = .{
            .base = .{
                .id = ast.generateNodeId(),
                .span = ast.SourceSpan.fromToken(self.peek()),
            },
            .data = .{
                .FunctionCall = .{
                    .callee = callee.?,
                    .arguments = try arguments.toOwnedSlice(),
                },
            },
        };

        return call_expr;
    }

    fn methodNameToTokenType(name: []const u8) ?token.TokenType {
        // Core and array
        if (std.mem.eql(u8, name, "type")) return .TYPE;
        if (std.mem.eql(u8, name, "length")) return .LENGTH;
        if (std.mem.eql(u8, name, "slice")) return .SLICE;
        if (std.mem.eql(u8, name, "push")) return .PUSH;
        if (std.mem.eql(u8, name, "pop")) return .POP;
        if (std.mem.eql(u8, name, "insert")) return .INSERT;
        if (std.mem.eql(u8, name, "remove")) return .REMOVE;

        // Type conversions
        if (std.mem.eql(u8, name, "string")) return .TOSTRING;
        if (std.mem.eql(u8, name, "int")) return .TOINT;
        if (std.mem.eql(u8, name, "float")) return .TOFLOAT;
        if (std.mem.eql(u8, name, "byte")) return .TOBYTE;

        // Control flow
        if (std.mem.eql(u8, name, "panic")) return .PANIC;
        if (std.mem.eql(u8, name, "assert")) return .ASSERT;

        return null;
    }

    pub fn parseStructInit(self: *Parser) ErrorList!?*ast.Expr {
        if (self.peek().type != .STRUCT_INSTANCE) {
            return null;
        }

        self.advance();

        if (self.peek().type != .IDENTIFIER) {
            return error.ExpectedIdentifier;
        }

        var struct_name = self.peek();
        self.advance();

        if (self.peek().type == .DOT) {
            const namespace = struct_name.lexeme;

            if (!self.module_namespaces.contains(namespace)) {
                return error.ExpectedIdentifier;
            }

            self.advance();

            if (self.peek().type != .IDENTIFIER) {
                return error.ExpectedIdentifier;
            }

            struct_name = self.peek();
            self.advance();
        }

        if (self.peek().type != .LEFT_BRACE) {
            return error.ExpectedLeftBrace;
        }
        self.advance();

        var fields = std.array_list.Managed(*ast.StructInstanceField).init(self.allocator);
        errdefer {
            for (fields.items) |field| {
                field.deinit(self.allocator);
                self.allocator.destroy(field);
            }
            fields.deinit();
        }

        while (self.peek().type != .RIGHT_BRACE) {
            while (self.peek().type == .NEWLINE) self.advance();

            if (self.peek().type != .IDENTIFIER) {
                return error.ExpectedIdentifier;
            }
            const field_name = self.peek();
            self.advance();

            if (self.peek().type != .ASSIGN) {
                return error.ExpectedAssignmentOperator;
            }
            self.advance();

            var value: *ast.Expr = undefined;
            if (self.peek().type == .STRUCT_INSTANCE) {
                if (try parseStructInit(self)) |struct_init| {
                    value = struct_init;
                } else {
                    value = try expression_parser.parseExpression(self) orelse return error.ExpectedExpression;
                }
            } else {
                value = try expression_parser.parseExpression(self) orelse return error.ExpectedExpression;
            }

            const field = try self.allocator.create(ast.StructInstanceField);
            field.* = .{
                .name = field_name,
                .value = value,
            };
            try fields.append(field);

            if (self.peek().type == .COMMA) {
                self.advance();
            }
            while (self.peek().type == .NEWLINE) self.advance();
        }

        self.advance();

        const struct_init = try self.allocator.create(ast.Expr);
        struct_init.* = .{
            .base = .{
                .id = ast.generateNodeId(),
                .span = ast.SourceSpan.fromToken(self.peek()),
            },
            .data = .{
                .StructLiteral = .{
                    .name = struct_name,
                    .fields = try fields.toOwnedSlice(),
                },
            },
        };
        return struct_init;
    }
    pub fn index(self: *Parser, array_expr: ?*ast.Expr, _: Precedence) ErrorList!?*ast.Expr {
        if (self.peek().type == .LEFT_BRACKET) {
            self.advance();
        }

        const index_expr = try expression_parser.parseExpression(self) orelse return error.ExpectedExpression;

        if (self.peek().type != .RIGHT_BRACKET) {
            index_expr.deinit(self.allocator);
            self.allocator.destroy(index_expr);
            return error.ExpectedRightBracket;
        }
        self.advance();

        if (self.peek().type == .ASSIGN) {
            self.advance();
            const value = try expression_parser.parseExpression(self) orelse return error.ExpectedExpression;

            const expr = try self.allocator.create(ast.Expr);
            expr.* = .{
                .base = .{
                    .id = ast.generateNodeId(),
                    .span = ast.SourceSpan.fromToken(self.peek()),
                },
                .data = .{
                    .IndexAssign = .{
                        .array = array_expr.?,
                        .index = index_expr,
                        .value = value,
                    },
                },
            };
            return expr;
        }

        const expr = try self.allocator.create(ast.Expr);
        expr.* = .{
            .base = .{
                .id = ast.generateNodeId(),
                .span = ast.SourceSpan.fromToken(self.peek()),
            },
            .data = .{
                .Index = .{
                    .array = array_expr.?,
                    .index = index_expr,
                },
            },
        };

        if (self.peek().type == .LEFT_BRACKET) {
            self.advance();
            return self.index(expr, .NONE);
        }

        return expr;
    }

    pub fn assignment(self: *Parser, left: ?*ast.Expr, _: Precedence) ErrorList!?*ast.Expr {
        if (left == null) return error.ExpectedExpression;

        if (self.previous().type != .ASSIGN) {
            return error.UseIsForAssignment;
        }

        const value = try expression_parser.parseExpression(self) orelse return error.ExpectedExpression;

        switch (left.?.data) {
            .Variable => |name| {
                const assign = try self.allocator.create(ast.Expr);
                assign.* = .{
                    .base = .{
                        .id = ast.generateNodeId(),
                        .span = ast.SourceSpan.fromToken(self.peek()),
                    },
                    .data = .{
                        .Assignment = .{
                            .name = name,
                            .value = value,
                        },
                    },
                };
                return assign;
            },
            .FieldAccess => |field_access| {
                const assign = try self.allocator.create(ast.Expr);
                assign.* = .{
                    .base = .{
                        .id = ast.generateNodeId(),
                        .span = ast.SourceSpan.fromToken(self.peek()),
                    },
                    .data = .{
                        .FieldAssignment = .{
                            .object = field_access.object,
                            .field = field_access.field,
                            .value = value,
                        },
                    },
                };
                return assign;
            },
            .Index => |index_expr| {
                const assign = try self.allocator.create(ast.Expr);
                assign.* = .{
                    .base = .{
                        .id = ast.generateNodeId(),
                        .span = ast.SourceSpan.fromToken(self.peek()),
                    },
                    .data = .{
                        .IndexAssign = .{
                            .array = index_expr.array,
                            .index = index_expr.index,
                            .value = value,
                        },
                    },
                };
                return assign;
            },
            else => return error.InvalidAssignmentTarget,
        }
    }

    pub fn fieldAccess(self: *Parser, left: ?*ast.Expr, _: Precedence) ErrorList!?*ast.Expr {
        if (self.peek().type == .DOT) {
            self.advance();
        }

        const current_token = self.peek();

        if (current_token.type != .IDENTIFIER and current_token.type != .FIELD_ACCESS) {
            return error.ExpectedIdentifier;
        }

        // Check if this is an enum member access
        if (left != null and left.?.data == .Variable) {
            const var_name = left.?.data.Variable;
            if (self.declared_types.contains(var_name.lexeme)) {
                // Check if this is actually an enum type by looking for enum declarations
                var is_enum_type = false;
                var i: usize = 0;
                while (i < self.tokens.len) : (i += 1) {
                    if (self.tokens[i].type == .ENUM_TYPE) {
                        i += 1;
                        if (i < self.tokens.len and std.mem.eql(u8, self.tokens[i].lexeme, var_name.lexeme)) {
                            is_enum_type = true;
                            break;
                        }
                    }
                }

                if (is_enum_type) {
                    // This is an enum member access
                    self.advance();

                    var found_valid_variant = false;
                    var j: usize = 0;
                    while (j < self.tokens.len) : (j += 1) {
                        if (self.tokens[j].type == .ENUM_TYPE) {
                            j += 2;

                            if (self.tokens[j].type != .LEFT_BRACE) continue;
                            j += 1;

                            while (j < self.tokens.len and self.tokens[j].type != .RIGHT_BRACE) : (j += 1) {
                                if (self.tokens[j].type == .IDENTIFIER) {
                                    if (std.mem.eql(u8, self.tokens[j].lexeme, current_token.lexeme)) {
                                        found_valid_variant = true;
                                        break;
                                    }
                                }
                            }
                            if (found_valid_variant) break;
                        }
                    }

                    if (found_valid_variant) {
                        const enum_member = try self.allocator.create(ast.Expr);
                        enum_member.* = .{
                            .base = .{
                                .id = ast.generateNodeId(),
                                .span = ast.SourceSpan.fromToken(self.peek()),
                            },
                            .data = .{
                                .EnumMember = current_token,
                            },
                        };
                        return enum_member;
                    }
                }
            }
        }

        self.advance();

        const field_access = try self.allocator.create(ast.Expr);
        field_access.* = .{
            .base = .{
                .id = ast.generateNodeId(),
                .span = ast.SourceSpan.fromToken(self.peek()),
            },
            .data = .{
                .FieldAccess = .{
                    .object = left.?,
                    .field = current_token,
                },
            },
        };

        if (self.peek().type == .LEFT_BRACKET) {
            self.advance();
            return try self.index(field_access, .NONE);
        }

        // Check if this field access is followed by parentheses - treat as function call
        if (self.peek().type == .LEFT_PAREN) {
            // Check if this is a reserved method name
            if (Parser.methodNameToTokenType(current_token.lexeme)) |_| {
                const loc: Location = .{ .file = self.current_file, .file_uri = self.current_file_uri, .range = .{ .start_line = current_token.line, .start_col = current_token.column, .end_line = current_token.line, .end_col = current_token.column } };
                self.reporter.reportCompileError(loc, ErrorCode.UNKNOWN_METHOD, "Unknown field or method '{s}'. If this is a compiler method, use @{s}(...)", .{ current_token.lexeme, current_token.lexeme });
                return error.UnknownFieldOrMethod;
            }
            return try self.call(field_access, .CALL);
        }

        return field_access;
    }

    pub fn internalCallExpr(self: *Parser, left: ?*ast.Expr, prec: Precedence) ErrorList!?*ast.Expr {
        const internal_call_parser = @import("internal_call_parser.zig");
        return internal_call_parser.internalCallExpr(self, left, prec);
    }

    pub fn print(self: *Parser, left: ?*ast.Expr, _: Precedence) ErrorList!?*ast.Expr {
        if (left == null) return error.ExpectedExpression;

        var name_token: ?token.Token = null;
        if (left.?.data == .Variable) {
            name_token = left.?.data.Variable;
        }

        const peek_expr = try self.allocator.create(ast.Expr);
        peek_expr.* = .{
            .base = .{
                .id = ast.generateNodeId(),
                .span = ast.SourceSpan.fromToken(self.peek()),
            },
            .data = .{
                .Peek = .{
                    .expr = left.?,
                    .location = .{
                        .file = self.current_file,
                        .file_uri = self.current_file_uri,
                        .range = .{
                            .start_line = @intCast(self.peek().line),
                            .start_col = self.peek().column - 1,
                            .end_line = @intCast(self.peek().line),
                            .end_col = self.peek().column + self.peek().lexeme.len - 1,
                        },
                    },
                    .variable_name = if (name_token) |token_name| token_name.lexeme else null,
                },
            },
        };

        return peek_expr;
    }

    pub fn enumMember(self: *Parser, _: ?*ast.Expr, _: Precedence) ErrorList!?*ast.Expr {
        self.advance();

        if (self.peek().type != .IDENTIFIER) {
            return error.ExpectedIdentifier;
        }

        const member = self.peek();
        self.advance();

        const enum_member = try self.allocator.create(ast.Expr);
        enum_member.* = .{
            .base = .{
                .id = ast.generateNodeId(),
                .span = ast.SourceSpan.fromToken(self.peek()),
            },
            .data = .{
                .EnumMember = member,
            },
        };
        return enum_member;
    }

    pub fn parseMap(self: *Parser) ErrorList!?*ast.Expr {
        var entries = std.array_list.Managed(*ast.MapEntry).init(self.allocator);
        errdefer {
            for (entries.items) |entry| {
                entry.deinit(self.allocator);
                self.allocator.destroy(entry);
            }
            entries.deinit();
        }

        var else_value: ?*ast.Expr = null;
        errdefer if (else_value) |ev| {
            ev.deinit(self.allocator);
            self.allocator.destroy(ev);
        };

        while (self.peek().type != .RIGHT_BRACE and self.peek().type != .EOF) {
            while (self.peek().type == .NEWLINE) self.advance();

            // Handle else clause
            if (self.peek().type == .ELSE) {
                if (else_value != null) return error.DuplicateElseClause;
                self.advance();

                else_value = try expression_parser.parseExpression(self) orelse return error.ExpectedExpression;

                if (self.peek().type == .COMMA) {
                    self.advance();
                }
                while (self.peek().type == .NEWLINE) self.advance();
                continue;
            }

            const key = blk: {
                const token_type = self.peek().type;
                switch (token_type) {
                    .INT, .FLOAT, .STRING, .BYTE, .LOGIC, .IDENTIFIER, .DOT => {
                        const prec = try precedence.parsePrecedence(self, Precedence.PRIMARY) orelse return error.ExpectedExpression;
                        break :blk prec;
                    },
                    else => return error.ExpectedMapKey,
                }
            };

            if (self.peek().type != .THEN) {
                key.deinit(self.allocator);
                self.allocator.destroy(key);
                return error.ExpectedThen;
            }
            self.advance();

            const value = try expression_parser.parseExpression(self) orelse return error.ExpectedExpression;

            const entry = try self.allocator.create(ast.MapEntry);
            entry.* = .{
                .key = key,
                .value = value,
            };
            try entries.append(entry);

            if (self.peek().type == .COMMA) {
                self.advance();
            }
            while (self.peek().type == .NEWLINE) self.advance();
        }

        if (self.peek().type != .RIGHT_BRACE) {
            return error.ExpectedRightBrace;
        }
        self.advance();

        const expr = try self.allocator.create(ast.Expr);
        expr.* = .{
            .base = .{
                .id = ast.generateNodeId(),
                .span = ast.SourceSpan.fromToken(self.peek()),
            },
            .data = if (else_value != null) .{
                .MapLiteral = .{
                    .entries = try entries.toOwnedSlice(),
                    .else_value = else_value,
                },
            } else .{
                .Map = try entries.toOwnedSlice(),
            },
        };
        return expr;
    }

    pub fn parseBlock(self: *Parser) ErrorList!?*ast.Expr {
        var statements = std.array_list.Managed(ast.Stmt).init(self.allocator);
        errdefer {
            for (statements.items) |*stmt| {
                stmt.deinit(self.allocator);
            }
            statements.deinit();
        }

        while (self.peek().type != .RIGHT_BRACE and self.peek().type != .EOF) {
            const stmt = try statement_parser.parseExpressionStmt(self);
            try statements.append(stmt);

            if (self.peek().type == .RIGHT_BRACE) {
                break;
            }
        }

        if (self.peek().type != .RIGHT_BRACE) {
            return error.ExpectedRightBrace;
        }
        self.advance();

        const block_expr = try self.allocator.create(ast.Expr);
        block_expr.* = .{
            .base = .{
                .id = ast.generateNodeId(),
                .span = ast.SourceSpan.fromToken(self.peek()),
            },
            .data = .{
                .Block = .{
                    .statements = try statements.toOwnedSlice(),
                    .value = null,
                },
            },
        };

        return block_expr;
    }

    fn reportWarning(self: *Parser, message: []const u8) void {
        _ = self;
        var reporting = Reporting.init();
        reporting.reportWarning("{s}", .{message});
    }

    fn check(self: *Parser, token_type: token.TokenType) bool {
        return self.peek().type == token_type;
    }

    fn isAlternateToken(token_type: token.TokenType) bool {
        return switch (token_type) {
            .ASSIGN_SYMBOL, .ASSIGN_KEYWORD, .EQUALITY_SYMBOL, .EQUALITY_KEYWORD, .AND_SYMBOL, .AND_KEYWORD, .OR_SYMBOL, .OR_KEYWORD, .WHERE_SYMBOL, .WHERE_KEYWORD, .FN_KEYWORD, .FUNCTION_KEYWORD => true,
            else => false,
        };
    }

    pub fn reportCircularImport(self: *Parser, current_module: []const u8) ErrorList!ast.ModuleInfo {
        var error_msg = std.array_list.Managed(u8).init(self.allocator);
        defer error_msg.deinit();

        try error_msg.appendSlice("Circular import detected:\n");

        for (self.import_stack.items) |entry| {
            try error_msg.appendSlice("  ");
            try error_msg.appendSlice(entry.module_path);
            try error_msg.appendSlice(" imports\n");
        }

        try error_msg.appendSlice("  ");
        try error_msg.appendSlice(current_module);
        try error_msg.appendSlice(" (circular dependency)\n");

        self.reporter.reportCompileError(null, null, "{s}", .{error_msg.items});

        return error.CircularImport;
    }

    const ModuleData = struct {
        source: []const u8,
        resolved_path: []const u8,
    };

    pub fn loadModuleSourceWithPath(self: *Parser, module_name: []const u8) ErrorList!ModuleData {
        var clean_name = module_name;
        if (std.mem.startsWith(u8, clean_name, "./")) {
            clean_name = clean_name[2..];
        }

        const has_doxa_ext = std.mem.endsWith(u8, clean_name, ".doxa");
        const has_extension = has_doxa_ext;

        const current_dir = std.fs.path.dirname(self.current_file) orelse ".";

        const readFileContentsWithPath = struct {
            fn read(alloc: std.mem.Allocator, file_path: []const u8) !ModuleData {
                var file = try std.fs.cwd().openFile(file_path, .{});
                defer file.close();

                const size = try file.getEndPos();
                const buffer = try alloc.alloc(u8, size);
                errdefer alloc.free(buffer);

                const bytes_read = try file.readAll(buffer);
                if (bytes_read != size) {
                    alloc.free(buffer);
                    return error.IncompleteRead;
                }

                const path_copy = try alloc.dupe(u8, file_path);

                return ModuleData{
                    .source = buffer,
                    .resolved_path = path_copy,
                };
            }
        }.read;

        var err: anyerror = error.FileNotFound;

        // 0a. Same directory as current file with potential existing extension
        if (has_extension) {
            var same_dir_exact_path_pre = std.array_list.Managed(u8).init(self.allocator);
            defer same_dir_exact_path_pre.deinit();
            try same_dir_exact_path_pre.appendSlice(current_dir);
            try same_dir_exact_path_pre.appendSlice("/");
            try same_dir_exact_path_pre.appendSlice(clean_name);
            if (readFileContentsWithPath(self.allocator, same_dir_exact_path_pre.items)) |data| {
                return data;
            } else |e| {
                err = e;
            }
        }

        // 0b. Same directory as current file with .doxa extension
        var same_dir_path_pre = std.array_list.Managed(u8).init(self.allocator);
        defer same_dir_path_pre.deinit();
        try same_dir_path_pre.appendSlice(current_dir);
        try same_dir_path_pre.appendSlice("/");
        try same_dir_path_pre.appendSlice(clean_name);
        if (!has_extension) try same_dir_path_pre.appendSlice(".doxa");
        if (readFileContentsWithPath(self.allocator, same_dir_path_pre.items)) |data| {
            return data;
        } else |e| {
            err = e;
        }

        // 1. First try directly with the given name (possibly including extension)
        if (has_extension) {
            var direct_path = std.array_list.Managed(u8).init(self.allocator);
            defer direct_path.deinit();
            try direct_path.appendSlice(clean_name);

            if (readFileContentsWithPath(self.allocator, direct_path.items)) |data| {
                return data;
            } else |e| {
                err = e;
            }
        }

        // 2. Try in CWD with .doxa extension
        var cwd_doxa_path = std.array_list.Managed(u8).init(self.allocator);
        defer cwd_doxa_path.deinit();
        try cwd_doxa_path.appendSlice(clean_name);
        if (!has_extension) try cwd_doxa_path.appendSlice(".doxa");

        if (readFileContentsWithPath(self.allocator, cwd_doxa_path.items)) |data| {
            return data;
        } else |e| {
            err = e;
        }

        // 3. Try the same directory as the current file with potential existing extension
        if (has_extension) {
            var same_dir_exact_path = std.array_list.Managed(u8).init(self.allocator);
            defer same_dir_exact_path.deinit();
            try same_dir_exact_path.appendSlice(current_dir);
            try same_dir_exact_path.appendSlice("/");
            try same_dir_exact_path.appendSlice(clean_name);

            if (readFileContentsWithPath(self.allocator, same_dir_exact_path.items)) |data| {
                return data;
            } else |e| {
                err = e;
            }
        }

        // 4. Try the same directory as the current file with .doxa extension
        var same_dir_path = std.array_list.Managed(u8).init(self.allocator);
        defer same_dir_path.deinit();
        try same_dir_path.appendSlice(current_dir);
        try same_dir_path.appendSlice("/");
        try same_dir_path.appendSlice(clean_name);
        if (!has_extension) try same_dir_path.appendSlice(".doxa");

        if (readFileContentsWithPath(self.allocator, same_dir_path.items)) |data| {
            return data;
        } else |e| {
            err = e;
        }

        // 5. Try the modules subdirectory (with potential existing extension)
        if (has_extension) {
            var modules_exact_path = std.array_list.Managed(u8).init(self.allocator);
            defer modules_exact_path.deinit();
            try modules_exact_path.appendSlice(current_dir);
            try modules_exact_path.appendSlice("/modules/");
            try modules_exact_path.appendSlice(clean_name);

            if (readFileContentsWithPath(self.allocator, modules_exact_path.items)) |data| {
                return data;
            } else |e| {
                err = e;
            }
        }

        // 6. Try the modules subdirectory with .doxa extension
        var modules_path = std.array_list.Managed(u8).init(self.allocator);
        defer modules_path.deinit();
        try modules_path.appendSlice(current_dir);
        try modules_path.appendSlice("/modules/");
        try modules_path.appendSlice(clean_name);
        if (!has_extension) try modules_path.appendSlice(".doxa");

        if (readFileContentsWithPath(self.allocator, modules_path.items)) |data| {
            return data;
        } else |e| {
            err = e;
        }

        return error.ModuleNotFound;
    }

    pub fn loadModuleSource(self: *Parser, module_name: []const u8) ErrorList![]const u8 {
        const data = try self.loadModuleSourceWithPath(module_name);
        return data.source;
    }

    pub fn arrayPush(self: *Parser, array: ?*ast.Expr, _: Precedence) ErrorList!?*ast.Expr {
        if (array == null) return error.ExpectedExpression;

        const element = try expression_parser.parseExpression(self) orelse return error.ExpectedExpression;

        const push_expr = try self.allocator.create(ast.Expr);
        push_expr.* = .{
            .base = .{
                .id = ast.generateNodeId(),
                .span = ast.SourceSpan.fromToken(self.peek()),
            },
            .data = .{
                .ArrayPush = .{
                    .array = array.?,
                    .element = element,
                },
            },
        };

        return push_expr;
    }

    pub fn arrayIsEmpty(self: *Parser, array: ?*ast.Expr, _: Precedence) ErrorList!?*ast.Expr {
        if (array == null) return error.ExpectedExpression;

        const empty_expr = try self.allocator.create(ast.Expr);
        empty_expr.* = .{
            .base = .{
                .id = ast.generateNodeId(),
                .span = ast.SourceSpan.fromToken(self.peek()),
            },
            .data = .{
                .ArrayIsEmpty = .{ .array = array.? },
            },
        };
        return empty_expr;
    }

    pub fn previous(self: *Parser) token.Token {
        if (self.current == 0) {
            return self.tokens[0];
        }
        return self.tokens[self.current - 1];
    }

    pub fn arrayPop(self: *Parser, array: ?*ast.Expr, _: Precedence) ErrorList!?*ast.Expr {
        if (array == null) return error.ExpectedExpression;

        const pop_expr = try self.allocator.create(ast.Expr);
        pop_expr.* = .{
            .base = .{
                .id = ast.generateNodeId(),
                .span = ast.SourceSpan.fromToken(self.peek()),
            },
            .data = .{
                .ArrayPop = .{
                    .array = array.?,
                },
            },
        };

        return pop_expr;
    }

    pub fn arrayConcat(self: *Parser, array: ?*ast.Expr, _: Precedence) ErrorList!?*ast.Expr {
        if (array == null) return error.ExpectedExpression;

        const array2 = try expression_parser.parseExpression(self) orelse return error.ExpectedExpression;

        const concat_expr = try self.allocator.create(ast.Expr);
        concat_expr.* = .{
            .base = .{
                .id = ast.generateNodeId(),
                .span = ast.SourceSpan.fromToken(self.peek()),
            },
            .data = .{
                .ArrayConcat = .{
                    .array = array.?,
                    .array2 = array2,
                },
            },
        };
        return concat_expr;
    }

    pub fn arrayLength(self: *Parser, array: ?*ast.Expr, _: Precedence) ErrorList!?*ast.Expr {
        if (array == null) return error.ExpectedExpression;

        const length_expr = try self.allocator.create(ast.Expr);
        length_expr.* = .{
            .base = .{
                .id = ast.generateNodeId(),
                .span = ast.SourceSpan.fromToken(self.peek()),
            },
            .data = .{
                .ArrayLength = .{
                    .array = array.?,
                },
            },
        };

        return length_expr;
    }

    pub fn input(self: *Parser, _: ?*ast.Expr, _: Precedence) ErrorList!?*ast.Expr {
        self.advance();

        var prompt: token.Token = token.Token{
            .type = .STRING,
            .lexeme = "",
            .literal = .{ .string = "" },
            .line = 0,
            .column = 0,
            .file = "",
            .file_uri = "",
        };
        if (self.peek().type == .LEFT_PAREN) {
            self.advance();

            if (self.peek().type == .RIGHT_PAREN) {
                self.advance();
            } else if (self.peek().type == .STRING) {
                prompt = self.peek();
                self.advance();
                if (self.peek().type != .RIGHT_PAREN) {
                    return error.ExpectedRightParen;
                }
                self.advance();
            } else {
                return error.ExpectedString;
            }
        } else if (self.peek().type == .STRING) {
            prompt = self.peek();
            self.advance();
        } else {
            prompt = token.Token{
                .file = self.current_file,
                .file_uri = self.current_file_uri,
                .type = .STRING,
                .lexeme = "",
                .literal = .{ .string = "" },
                .line = self.peek().line,
                .column = self.peek().column,
            };
        }

        const input_expr = try self.allocator.create(ast.Expr);
        input_expr.* = .{
            .base = .{
                .id = ast.generateNodeId(),
                .span = ast.SourceSpan.fromToken(self.peek()),
            },
            .data = .{
                .Input = .{
                    .prompt = prompt,
                },
            },
        };

        return input_expr;
    }

    pub fn loadAndRegisterModule(self: *Parser, module_path: []const u8, namespace: []const u8, specific_symbol: ?[]const u8) ErrorList!void {
        const module_info = try module_resolver.resolveModule(self, module_path);

        try self.module_namespaces.put(namespace, module_info);

        if (module_info.imports.len > 0) {
            for (module_info.imports) |import| {
                if (import.namespace_alias) |alias| {
                    if (std.mem.eql(u8, import.module_path, self.current_file)) continue;

                    const qualified_alias = try std.fmt.allocPrint(self.allocator, "{s}.{s}", .{ namespace, alias });
                    defer self.allocator.free(qualified_alias);

                    if (!self.module_namespaces.contains(alias)) {
                        if (self.module_cache.contains(import.module_path)) {
                            const cached_module = self.module_cache.get(import.module_path).?;
                            try self.module_namespaces.put(alias, cached_module);
                        } else {
                            const previous_current_file = self.current_file;
                            const previous_current_file_uri = self.current_file_uri;
                            self.current_file = module_info.file_path;
                            self.current_file_uri = try self.reporter.ensureFileUri(module_info.file_path);
                            defer {
                                self.current_file = previous_current_file;
                                self.current_file_uri = previous_current_file_uri;
                            }
                            try self.loadAndRegisterModule(import.module_path, alias, import.specific_symbol);
                        }
                    }

                    try module_resolver.recordModuleImport(self, module_path, alias, import.module_path);
                }
            }
        }

        if (module_info.ast != null and module_info.symbols != null) {
            var it = module_info.symbols.?.iterator();
            while (it.next()) |entry| {
                const symbol = entry.value_ptr.*;
                const symbol_name = entry.key_ptr.*;

                if (symbol.is_public) {
                    if (specific_symbol) |specific| {
                        if (std.mem.eql(u8, specific, symbol_name)) {
                            const full_name = try std.fmt.allocPrint(self.allocator, "{s}.{s}", .{ namespace, symbol_name });

                            try self.imported_symbols.?.put(full_name, .{
                                .kind = switch (symbol.kind) {
                                    .Function => .Function,
                                    .Variable => .Variable,
                                    .Struct => .Struct,
                                    .Enum => .Enum,
                                },
                                .name = symbol_name,
                                .original_module = module_path,
                            });
                        }
                    } else {
                        const full_name = try std.fmt.allocPrint(self.allocator, "{s}.{s}", .{ namespace, symbol_name });

                        try self.imported_symbols.?.put(full_name, .{
                            .kind = switch (symbol.kind) {
                                .Function => .Function,
                                .Variable => .Variable,
                                .Struct => .Struct,
                                .Enum => .Enum,
                            },
                            .name = symbol_name,
                            .original_module = module_path,
                        });
                    }
                }
            }
        }
    }

    pub fn loadAndRegisterSpecificSymbol(self: *Parser, module_path: []const u8, symbol_name: []const u8) ErrorList!void {
        const deriveAlias = struct {
            fn fromPath(allocator: std.mem.Allocator, path: []const u8) []const u8 {
                var it = std.mem.splitSequence(u8, path, "/");
                var last: []const u8 = path;
                while (it.next()) |part| last = part;
                if (std.mem.endsWith(u8, last, ".doxa")) {
                    return allocator.dupe(u8, last[0 .. last.len - 5]) catch last;
                }
                return allocator.dupe(u8, last) catch last;
            }
        };

        if (self.module_cache.get(module_path)) |module_info| {
            const alias = deriveAlias.fromPath(self.allocator, module_path);
            if (!self.module_namespaces.contains(alias)) {
                try self.module_namespaces.put(alias, module_info);
            }

            if (module_info.ast) |module_ast| {
                try self.registerSpecificSymbol(module_ast, module_path, symbol_name);
            }
            return;
        }

        const module_info = try module_resolver.resolveModule(self, module_path);

        const alias = deriveAlias.fromPath(self.allocator, module_path);
        if (!self.module_namespaces.contains(alias)) {
            try self.module_namespaces.put(alias, module_info);
        }

        if (module_info.ast) |module_ast| {
            try self.registerSpecificSymbol(module_ast, module_path, symbol_name);
        }
    }

    fn registerSpecificSymbol(self: *Parser, module_ast: *ast.Expr, module_path: []const u8, symbol_name: []const u8) !void {
        if (self.imported_symbols == null) {
            self.imported_symbols = std.StringHashMap(import_parser.ImportedSymbol).init(self.allocator);
        }

        switch (module_ast.data) {
            .Block => {
                const statements = module_ast.data.Block.statements;
                for (statements) |stmt| {
                    switch (stmt.data) {
                        .FunctionDecl => |func| {
                            const is_public = func.is_public;
                            if (is_public and std.mem.eql(u8, func.name.lexeme, symbol_name)) {
                                var param_types: ?[]ast.TypeInfo = null;
                                if (func.params.len > 0) {
                                    param_types = try self.allocator.alloc(ast.TypeInfo, func.params.len);
                                    for (func.params, 0..) |param, i| {
                                        if (param.type_expr) |type_expr| {
                                            const type_info_ptr = try ast.typeInfoFromExpr(self.allocator, type_expr);
                                            param_types.?[i] = type_info_ptr.*;
                                            self.allocator.destroy(type_info_ptr);
                                        } else {
                                            param_types.?[i] = .{ .base = .Nothing };
                                        }
                                    }
                                }

                                try self.imported_symbols.?.put(symbol_name, .{
                                    .kind = .Function,
                                    .name = func.name.lexeme,
                                    .original_module = module_path,
                                    .namespace_alias = null,
                                    .param_count = @intCast(func.params.len),
                                    .param_types = param_types,
                                    .return_type_info = func.return_type_info,
                                });
                                return;
                            }
                        },
                        .VarDecl => |var_decl| {
                            const is_public = var_decl.is_public;
                            if (is_public and std.mem.eql(u8, var_decl.name.lexeme, symbol_name)) {
                                try self.imported_symbols.?.put(symbol_name, .{
                                    .kind = .Variable,
                                    .name = var_decl.name.lexeme,
                                    .original_module = module_path,
                                });
                                return;
                            }
                        },
                        .EnumDecl => |enum_decl| {
                            const is_public = enum_decl.is_public;
                            if (is_public and std.mem.eql(u8, enum_decl.name.lexeme, symbol_name)) {
                                try self.imported_symbols.?.put(symbol_name, .{
                                    .kind = .Enum,
                                    .name = enum_decl.name.lexeme,
                                    .original_module = module_path,
                                });
                                return;
                            }
                        },
                        .Expression => |maybe_expr| {
                            if (maybe_expr) |expr| {
                                if (expr.data == .StructDecl) {
                                    const struct_decl = expr.data.StructDecl;
                                    const is_public = struct_decl.is_public;
                                    if (is_public and std.mem.eql(u8, struct_decl.name.lexeme, symbol_name)) {
                                        try self.imported_symbols.?.put(symbol_name, .{
                                            .kind = .Struct,
                                            .name = struct_decl.name.lexeme,
                                            .original_module = module_path,
                                        });
                                        return;
                                    }
                                }
                            }
                        },
                        .Block, .MapDecl, .Return, .MapLiteral, .Module, .Import, .Path, .Continue, .Break, .Assert, .Cast => {},
                    }
                }
            },
            else => {},
        }
    }
};
