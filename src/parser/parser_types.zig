const std = @import("std");
const token = @import("../types/token.zig");
const declaration_parser = @import("declaration_parser.zig");
const inline_zig = @import("inline_zig.zig");
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

pub const ModuleImportEntry = struct {
    imported_path: []const u8,
    is_public: bool,
};

pub const SpecificImportEntry = struct {
    importer_path: []const u8,
    module_path: []const u8,
    symbol_name: []const u8,
    is_public: bool,
};

const PendingModuleDependency = struct {
    module_path: []const u8,
    alias: []const u8,
    parent_file: []const u8,
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

    module_imports: std.StringHashMap(std.StringHashMap(ModuleImportEntry)),
    specific_imports: std.array_list.Managed(SpecificImportEntry),

    imported_symbols: ?std.StringHashMap(import_parser.ImportedSymbol) = null,

    declared_types: std.StringHashMap(void),

    module_resolution_status: std.StringHashMap(ModuleResolutionStatus),
    import_stack: std.array_list.Managed(ImportStackEntry),

    // Match path pattern tracking
    current_path_pattern_tokens: ?[]const token.Token = null,
    current_path_pattern_is_wildcard: bool = false,
    current_path_pattern_field_names: ?[]const token.Token = null,

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
            .module_imports = std.StringHashMap(std.StringHashMap(ModuleImportEntry)).init(allocator),
            .specific_imports = std.array_list.Managed(SpecificImportEntry).init(allocator),
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
        self.specific_imports.deinit();
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
        while (start_index_after_imports < self.tokens.len) {
            var pass_is_public = false;
            var token_type = self.tokens[start_index_after_imports].type;
            if (token_type == .PUBLIC and start_index_after_imports + 1 < self.tokens.len) {
                const next_type = self.tokens[start_index_after_imports + 1].type;
                if (next_type == .IMPORT or next_type == .MODULE) {
                    pass_is_public = true;
                    token_type = next_type;
                } else {
                    break;
                }
            }
            if (token_type != .IMPORT and token_type != .MODULE) break;

            const temp_current = self.current;
            self.current = start_index_after_imports;
            if (pass_is_public) {
                self.advance();
            }

            if (token_type == .MODULE) {
                _ = try import_parser.parseModuleStmt(self, pass_is_public);
            } else {
                _ = try import_parser.parseImportStmt(self, pass_is_public);
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
                .ZIG => {
                    if (is_entry) {
                        return error.MisplacedEntryPoint;
                    }
                    if (is_public) {
                        return error.MisplacedPublicModifier;
                    }
                    const zig_decl = try declaration_parser.parseZigDecl(self);
                    // Validate and register module namespace + function signatures so `Ops.fn(...)` works.
                    const module_name = zig_decl.data.ZigDecl.name.lexeme;
                    const zig_source = zig_decl.data.ZigDecl.source;

                    const sigs = try inline_zig.sanitizeAndExtract(self.allocator, zig_source);
                    defer inline_zig.deinitSigsShallow(self.allocator, sigs);

                    // Ensure the namespace exists (placeholder ModuleInfo is enough for semantic module lookup).
                    if (!self.module_namespaces.contains(module_name)) {
                        try self.module_namespaces.put(module_name, .{
                            .name = module_name,
                            .imports = &[_]ast.ImportInfo{},
                            .ast = null,
                            .file_path = self.current_file,
                            .symbols = null,
                        });
                    }

                    if (self.imported_symbols == null) {
                        self.imported_symbols = std.StringHashMap(import_parser.ImportedSymbol).init(self.allocator);
                    }

                    for (sigs) |sig| {
                        const full_name = try std.fmt.allocPrint(self.allocator, "{s}.{s}", .{ module_name, sig.name });
                        // Note: allocate-owned strings for imported symbol fields.
                        try self.imported_symbols.?.put(full_name, .{
                            .kind = .Function,
                            .name = sig.name,
                            .original_module = module_name,
                            .namespace_alias = null,
                            .param_count = @intCast(sig.param_types.len),
                            .param_types = sig.param_types,
                            .return_type_info = sig.return_type,
                        });
                    }

                    try statements.append(zig_decl);
                },
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
                    _ = try import_parser.parseImportStmt(self, is_public);
                },
                .MODULE => {
                    if (is_entry) {
                        return error.MisplacedEntryPoint;
                    }
                    _ = try import_parser.parseModuleStmt(self, is_public);
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
                .GROUP_TYPE => {
                    var group_decl = try declaration_parser.parseGroupDecl(self);
                    group_decl.data.GroupDecl.is_public = is_public;
                    if (is_entry) {
                        return error.InvalidEntryPoint;
                    }
                    try statements.append(group_decl);
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

        // System / introspection
        if (std.mem.eql(u8, name, "std")) return .STD;

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

            var builder = std.array_list.Managed(u8).init(self.allocator);
            errdefer builder.deinit();
            try builder.appendSlice(struct_name.lexeme);

            while (self.peek().type == .DOT) {
                self.advance();
                if (self.peek().type != .IDENTIFIER) {
                    return error.ExpectedIdentifier;
                }
                try builder.append('.');
                try builder.appendSlice(self.peek().lexeme);
                self.advance();
            }

            const qualified_lexeme = try builder.toOwnedSlice();
            struct_name = token.Token{
                .type = .IDENTIFIER,
                .lexeme = qualified_lexeme,
                .literal = .nothing,
                .line = struct_name.line,
                .column = struct_name.column,
                .file = struct_name.file,
                .file_uri = struct_name.file_uri,
            };
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
                // Check if this is actually an enum or group type by looking for declarations
                var is_enum_type = false;
                var i: usize = 0;
                while (i < self.tokens.len) : (i += 1) {
                    if (self.tokens[i].type == .ENUM_TYPE or self.tokens[i].type == .GROUP_TYPE) {
                        i += 1;
                        if (i < self.tokens.len and std.mem.eql(u8, self.tokens[i].lexeme, var_name.lexeme)) {
                            is_enum_type = true;
                            break;
                        }
                    }
                }

                if (is_enum_type) {
                    // This is an enum/group member access
                    self.advance();

                    var found_valid_variant = false;
                    var j: usize = 0;
                    while (j < self.tokens.len) : (j += 1) {
                        if (self.tokens[j].type == .ENUM_TYPE or self.tokens[j].type == .GROUP_TYPE) {
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
                    .INT, .FLOAT, .STRING, .SINGLE_STRING, .BYTE, .LOGIC, .IDENTIFIER, .DOT => {
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
                    .key_type = null,
                    .value_type = null,
                    .else_value = else_value,
                },
            } else .{
                .Map = .{
                    .entries = try entries.toOwnedSlice(),
                    .key_type = null,
                    .value_type = null,
                },
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
        return module_resolver.reportCircularImport(self, current_module);
    }

    pub fn loadModuleSourceWithPath(self: *Parser, module_name: []const u8) ErrorList!module_resolver.ModuleData {
        return module_resolver.loadModuleSourceWithPath(self, module_name);
    }

    pub fn loadModuleSource(self: *Parser, module_name: []const u8) ErrorList![]const u8 {
        return module_resolver.loadModuleSource(self, module_name);
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

    pub fn registerModuleAlias(self: *Parser, importer_path: []const u8, namespace: []const u8, module_path: []const u8, is_public: bool) ErrorList!void {
        try module_resolver.recordModuleImport(self, importer_path, namespace, module_path, is_public);
        if (!self.module_namespaces.contains(namespace)) {
            try self.module_namespaces.put(namespace, .{
                .name = namespace,
                .imports = &[_]ast.ImportInfo{},
                .ast = null,
                .file_path = module_path,
                .symbols = null,
            });
        }
    }

    pub fn recordSpecificImport(self: *Parser, importer_path: []const u8, module_path: []const u8, symbol_name: []const u8, is_public: bool) ErrorList!void {
        for (self.specific_imports.items) |entry| {
            if (std.mem.eql(u8, entry.importer_path, importer_path) and
                std.mem.eql(u8, entry.module_path, module_path) and
                std.mem.eql(u8, entry.symbol_name, symbol_name))
            {
                return;
            }
        }

        try self.specific_imports.append(.{
            .importer_path = importer_path,
            .module_path = module_path,
            .symbol_name = symbol_name,
            .is_public = is_public,
        });
    }

    pub fn ensureModuleNamespace(self: *Parser, namespace: []const u8) ErrorList!?ast.ModuleInfo {
        if (self.module_namespaces.get(namespace)) |existing| {
            if (existing.ast != null) return existing;
            const module_info = try self.loadAndRegisterModule(existing.file_path, namespace, null);
            return module_info;
        }
        return null;
    }

    pub fn ensureNestedModuleNamespace(self: *Parser, module_name: []const u8, field_name: []const u8) ErrorList!?ast.ModuleInfo {
        const parent_info = (try self.ensureModuleNamespace(module_name)) orelse return null;
        for (parent_info.imports) |import| {
            if (!import.is_public or import.import_type != .Module) continue;
            const alias = import.namespace_alias orelse continue;
            if (!std.mem.eql(u8, alias, field_name)) continue;

            const qualified_alias = try std.fmt.allocPrint(self.allocator, "{s}.{s}", .{ module_name, field_name });
            if (try self.namespaceChainContainsImport(module_name, parent_info.file_path, import.module_path)) {
                _ = self.reportCircularImport(import.module_path) catch {};
                return error.CircularImport;
            }
            if (self.module_namespaces.get(qualified_alias)) |existing| {
                if (existing.ast != null) return existing;
            }

            const previous_current_file = self.current_file;
            const previous_current_file_uri = self.current_file_uri;
            self.current_file = parent_info.file_path;
            self.current_file_uri = try self.reporter.ensureFileUri(parent_info.file_path);
            defer {
                self.current_file = previous_current_file;
                self.current_file_uri = previous_current_file_uri;
            }

            const child_info = try self.loadAndRegisterModule(import.module_path, qualified_alias, import.specific_symbol);
            return child_info;
        }
        return null;
    }

    fn namespaceChainContainsImport(self: *Parser, module_name: []const u8, importer_file: []const u8, import_path: []const u8) !bool {
        const target_path = try normalizeImportPathForCycleCheck(self.allocator, importer_file, import_path);
        defer self.allocator.free(target_path);

        var current_name = module_name;
        while (true) {
            if (self.module_namespaces.get(current_name)) |module_info| {
                const current_path = try normalizeImportPathForCycleCheck(self.allocator, ".", module_info.file_path);
                defer self.allocator.free(current_path);
                if (std.mem.eql(u8, current_path, target_path)) return true;
            }

            const dot_idx = std.mem.lastIndexOfScalar(u8, current_name, '.') orelse break;
            current_name = current_name[0..dot_idx];
        }

        return false;
    }

    fn normalizeImportPathForCycleCheck(allocator: std.mem.Allocator, importer_file: []const u8, module_path: []const u8) ![]const u8 {
        var clean_path = module_path;
        if (std.mem.startsWith(u8, clean_path, "./")) {
            clean_path = clean_path[2..];
        }
        if (!std.mem.endsWith(u8, clean_path, ".doxa")) {
            clean_path = try std.fmt.allocPrint(allocator, "{s}.doxa", .{clean_path});
        } else {
            clean_path = try allocator.dupe(u8, clean_path);
        }
        defer allocator.free(clean_path);

        if (std.fs.path.isAbsolute(clean_path)) {
            return try std.fs.path.resolve(allocator, &.{clean_path});
        }

        const base_dir = std.fs.path.dirname(importer_file) orelse ".";
        return try std.fs.path.resolve(allocator, &.{ base_dir, clean_path });
    }

    pub fn ensureImportedSymbol(self: *Parser, symbol_name: []const u8) ErrorList!bool {
        if (self.imported_symbols) |symbols| {
            if (symbols.contains(symbol_name)) return true;
        }

        var matched = false;
        for (self.specific_imports.items) |import_entry| {
            if (!std.mem.eql(u8, import_entry.symbol_name, symbol_name)) continue;
            matched = true;

            {
                const previous_current_file = self.current_file;
                const previous_current_file_uri = self.current_file_uri;
                self.current_file = import_entry.importer_path;
                self.current_file_uri = try self.reporter.ensureFileUri(import_entry.importer_path);
                defer {
                    self.current_file = previous_current_file;
                    self.current_file_uri = previous_current_file_uri;
                }
                try self.loadAndRegisterSpecificSymbol(import_entry.module_path, import_entry.symbol_name);
            }

            if (self.imported_symbols) |symbols| {
                if (symbols.contains(symbol_name)) return true;
            }
        }

        if (!matched) return false;
        return if (self.imported_symbols) |symbols| symbols.contains(symbol_name) else false;
    }

    pub fn ensureSpecificImports(self: *Parser) ErrorList!void {
        var import_index: usize = 0;
        while (import_index < self.specific_imports.items.len) : (import_index += 1) {
            const import_entry = self.specific_imports.items[import_index];
            _ = try self.ensureImportedSymbol(import_entry.symbol_name);
        }
    }

    pub fn ensureReachableModuleDependencies(self: *Parser) ErrorList!void {
        var made_progress = true;
        while (made_progress) {
            made_progress = false;

            var pending = std.array_list.Managed(PendingModuleDependency).init(self.allocator);
            defer pending.deinit();

            var it = self.module_namespaces.iterator();
            while (it.next()) |entry| {
                const module_info = entry.value_ptr.*;
                if (!moduleContributesBodies(module_info)) continue;

                for (module_info.imports) |import| {
                    switch (import.import_type) {
                        .Module => {
                            if (!moduleReferencesImport(module_info, import)) continue;
                            const alias = import.namespace_alias orelse continue;
                            if (self.module_namespaces.get(alias)) |existing| {
                                if (existing.ast != null) continue;
                            }
                            try pending.append(.{
                                .module_path = import.module_path,
                                .alias = alias,
                                .parent_file = module_info.file_path,
                            });
                        },
                        .Specific => {
                            if (import.specific_symbols) |symbols| {
                                for (symbols) |symbol_name| {
                                    if (!moduleReferencesName(module_info, symbol_name)) continue;
                                    try self.recordSpecificImport(module_info.file_path, import.module_path, symbol_name, import.is_public);
                                }
                            } else if (import.specific_symbol) |symbol_name| {
                                if (!moduleReferencesName(module_info, symbol_name)) continue;
                                try self.recordSpecificImport(module_info.file_path, import.module_path, symbol_name, import.is_public);
                            }
                        },
                    }
                }
            }

            try self.ensureSpecificImports();

            for (pending.items) |dep| {
                if (self.module_namespaces.get(dep.alias)) |existing| {
                    if (existing.ast != null) continue;
                }

                {
                    const previous_current_file = self.current_file;
                    const previous_current_file_uri = self.current_file_uri;
                    self.current_file = dep.parent_file;
                    self.current_file_uri = try self.reporter.ensureFileUri(dep.parent_file);
                    defer {
                        self.current_file = previous_current_file;
                        self.current_file_uri = previous_current_file_uri;
                    }

                    _ = try self.loadAndRegisterModule(dep.module_path, dep.alias, null);
                }
                made_progress = true;
            }
        }
    }

    fn moduleReferencesImport(module_info: ast.ModuleInfo, import: ast.ImportInfo) bool {
        return switch (import.import_type) {
            .Module => if (import.namespace_alias) |alias| moduleReferencesName(module_info, alias) else false,
            .Specific => blk: {
                if (import.specific_symbols) |symbols| {
                    for (symbols) |symbol_name| {
                        if (moduleReferencesName(module_info, symbol_name)) break :blk true;
                    }
                    break :blk false;
                }
                if (import.specific_symbol) |symbol_name| {
                    break :blk moduleReferencesName(module_info, symbol_name);
                }
                break :blk false;
            },
        };
    }

    fn moduleReferencesName(module_info: ast.ModuleInfo, name: []const u8) bool {
        const module_ast = module_info.ast orelse return false;
        if (module_ast.data != .Block) return false;
        return statementsReferenceName(module_ast.data.Block.statements, name);
    }

    fn statementsReferenceName(statements: []ast.Stmt, name: []const u8) bool {
        for (statements) |stmt| {
            if (stmtReferencesName(stmt, name)) return true;
        }
        return false;
    }

    fn stmtReferencesName(stmt: ast.Stmt, name: []const u8) bool {
        return switch (stmt.data) {
            .Expression => |maybe_expr| if (maybe_expr) |expr| exprReferencesName(expr, name) else false,
            .VarDecl => |decl| if (decl.initializer) |initializer| exprReferencesName(initializer, name) else false,
            .Block => |statements| statementsReferenceName(statements, name),
            .FunctionDecl => |func| statementsReferenceName(func.body, name),
            .Return => |ret| if (ret.value) |value| exprReferencesName(value, name) else false,
            .MapDecl => |decl| if (decl.else_value) |else_value| exprReferencesName(else_value, name) else false,
            .MapLiteral => |lit| blk: {
                for (lit.entries) |entry| {
                    if (exprReferencesName(entry.key, name) or exprReferencesName(entry.value, name)) break :blk true;
                }
                if (lit.else_value) |else_value| {
                    if (exprReferencesName(else_value, name)) break :blk true;
                }
                break :blk false;
            },
            .Assert => |assert_stmt| exprReferencesName(assert_stmt.condition, name) or
                (if (assert_stmt.message) |message| exprReferencesName(message, name) else false),
            .Cast => |cast| exprReferencesName(cast.value, name) or
                (if (cast.then_branch) |then_expr| exprReferencesName(then_expr, name) else false) or
                (if (cast.else_branch) |else_expr| exprReferencesName(else_expr, name) else false),
            else => false,
        };
    }

    fn exprReferencesName(expr: *const ast.Expr, name: []const u8) bool {
        return switch (expr.data) {
            .Variable => |tok| std.mem.eql(u8, tok.lexeme, name),
            .Binary => |binary| (if (binary.left) |left| exprReferencesName(left, name) else false) or
                (if (binary.right) |right| exprReferencesName(right, name) else false),
            .Unary => |unary| if (unary.right) |right| exprReferencesName(right, name) else false,
            .Peek => |peek_expr| exprReferencesName(peek_expr.expr, name),
            .InterpolatedString => |template| blk: {
                for (template.parts) |part| {
                    switch (part) {
                        .Expression => |part_expr| if (exprReferencesName(part_expr, name)) break :blk true,
                        .String => {},
                    }
                }
                break :blk false;
            },
            .Print => |print_expr| exprReferencesName(print_expr.expr, name),
            .PeekStruct => |peek_struct| exprReferencesName(peek_struct.expr, name),
            .Assignment => |assign_expr| std.mem.eql(u8, assign_expr.name.lexeme, name) or
                (if (assign_expr.value) |value| exprReferencesName(value, name) else false),
            .Grouping => |maybe_inner| if (maybe_inner) |inner| exprReferencesName(inner, name) else false,
            .If => |if_expr| (if (if_expr.condition) |condition| exprReferencesName(condition, name) else false) or
                (if (if_expr.then_branch) |then_branch| exprReferencesName(then_branch, name) else false) or
                (if (if_expr.else_branch) |else_branch| exprReferencesName(else_branch, name) else false),
            .Block => |block_expr| statementsReferenceName(block_expr.statements, name) or
                (if (block_expr.value) |value| exprReferencesName(value, name) else false),
            .Array => |items| blk: {
                for (items) |item| {
                    if (exprReferencesName(item, name)) break :blk true;
                }
                break :blk false;
            },
            .Struct => |fields| blk: {
                for (fields) |field| {
                    if (exprReferencesName(field.value, name)) break :blk true;
                }
                break :blk false;
            },
            .Index => |index_expr| exprReferencesName(index_expr.array, name) or exprReferencesName(index_expr.index, name),
            .IndexAssign => |assign| exprReferencesName(assign.array, name) or
                exprReferencesName(assign.index, name) or
                exprReferencesName(assign.value, name),
            .FunctionCall => |call_expr| blk: {
                if (exprReferencesName(call_expr.callee, name)) break :blk true;
                for (call_expr.arguments) |arg| {
                    if (exprReferencesName(arg.expr, name)) break :blk true;
                }
                break :blk false;
            },
            .Logical => |logical| exprReferencesName(logical.left, name) or exprReferencesName(logical.right, name),
            .FieldAccess => |field| exprReferencesName(field.object, name),
            .StructLiteral => |literal| blk: {
                if (std.mem.eql(u8, literal.name.lexeme, name)) break :blk true;
                for (literal.fields) |field| {
                    if (exprReferencesName(field.value, name)) break :blk true;
                }
                break :blk false;
            },
            .FieldAssignment => |field_assignment| exprReferencesName(field_assignment.object, name) or exprReferencesName(field_assignment.value, name),
            .Exists => |exists| exprReferencesName(exists.array, name) or exprReferencesName(exists.condition, name),
            .ForAll => |forall| exprReferencesName(forall.array, name) or exprReferencesName(forall.condition, name),
            .Match => |match_expr| blk: {
                if (exprReferencesName(match_expr.value, name)) break :blk true;
                for (match_expr.cases) |case| {
                    if (exprReferencesName(case.body, name)) break :blk true;
                }
                break :blk false;
            },
            .EnumMember => |tok| std.mem.eql(u8, tok.lexeme, name),
            .BuiltinCall => |builtin_call| blk: {
                for (builtin_call.arguments) |arg| {
                    if (exprReferencesName(arg, name)) break :blk true;
                }
                break :blk false;
            },
            .Map => |map_expr| mapEntriesReferenceName(map_expr.entries, name),
            .MapLiteral => |map_expr| mapEntriesReferenceName(map_expr.entries, name) or
                (if (map_expr.else_value) |else_value| exprReferencesName(else_value, name) else false),
            .InternalCall => |internal_call| blk: {
                if (exprReferencesName(internal_call.receiver, name)) break :blk true;
                for (internal_call.arguments) |arg| {
                    if (exprReferencesName(arg, name)) break :blk true;
                }
                break :blk false;
            },
            .Increment => |inner| exprReferencesName(inner, name),
            .Decrement => |inner| exprReferencesName(inner, name),
            .CompoundAssign => |assign| std.mem.eql(u8, assign.name.lexeme, name) or
                (if (assign.value) |value| exprReferencesName(value, name) else false),
            .Assert => |assert_expr| exprReferencesName(assert_expr.condition, name) or
                (if (assert_expr.message) |message| exprReferencesName(message, name) else false),
            .Cast => |cast| exprReferencesName(cast.value, name) or
                (if (cast.then_branch) |then_expr| exprReferencesName(then_expr, name) else false) or
                (if (cast.else_branch) |else_expr| exprReferencesName(else_expr, name) else false),
            .ReturnExpr => |ret| if (ret.value) |value| exprReferencesName(value, name) else false,
            .Loop => |loop| (if (loop.var_decl) |var_decl| stmtReferencesName(var_decl.*, name) else false) or
                (if (loop.condition) |condition| exprReferencesName(condition, name) else false) or
                (if (loop.step) |step| exprReferencesName(step, name) else false) or
                exprReferencesName(loop.body, name),
            .Range => |range| exprReferencesName(range.start, name) or exprReferencesName(range.end, name),
            else => false,
        };
    }

    fn mapEntriesReferenceName(entries: []*ast.MapEntry, name: []const u8) bool {
        for (entries) |entry| {
            if (exprReferencesName(entry.key, name) or exprReferencesName(entry.value, name)) return true;
        }
        return false;
    }

    fn moduleContributesBodies(module_info: ast.ModuleInfo) bool {
        const module_ast = module_info.ast orelse return false;
        if (module_ast.data != .Block) return false;
        for (module_ast.data.Block.statements) |stmt| {
            switch (stmt.data) {
                .FunctionDecl, .VarDecl, .ZigDecl => return true,
                else => {},
            }
        }
        return false;
    }

    pub fn collectReachableModuleNamespaces(self: *Parser, allocator: std.mem.Allocator) !std.StringHashMap(ModuleInfo) {
        var reachable = std.StringHashMap(ModuleInfo).init(allocator);
        var it = self.module_namespaces.iterator();
        while (it.next()) |entry| {
            const module_info = entry.value_ptr.*;
            if (module_info.ast == null) continue;
            try reachable.put(entry.key_ptr.*, module_info);
        }
        return reachable;
    }

    pub fn loadAndRegisterModule(self: *Parser, module_path: []const u8, namespace: []const u8, specific_symbol: ?[]const u8) ErrorList!ast.ModuleInfo {
        const module_info = try module_resolver.resolveModule(self, module_path);

        try self.module_namespaces.put(namespace, module_info);
        try self.registerPublicSymbols(module_info, module_path, namespace, specific_symbol);

        return module_info;
    }

    fn registerPublicSymbols(self: *Parser, module_info: ast.ModuleInfo, module_path: []const u8, namespace: []const u8, specific_symbol: ?[]const u8) ErrorList!void {
        if (self.imported_symbols == null) {
            self.imported_symbols = std.StringHashMap(import_parser.ImportedSymbol).init(self.allocator);
        }
        if (module_info.symbols == null) return;

        var it = module_info.symbols.?.iterator();
        while (it.next()) |entry| {
            const symbol = entry.value_ptr.*;
            const symbol_name = entry.key_ptr.*;
            if (!symbol.is_public) continue;
            if (specific_symbol) |specific| {
                if (!std.mem.eql(u8, specific, symbol_name)) continue;
            }

            const full_name = try std.fmt.allocPrint(self.allocator, "{s}.{s}", .{ namespace, symbol_name });
            try self.imported_symbols.?.put(full_name, .{
                .kind = switch (symbol.kind) {
                    .Function => .Function,
                    .Variable => .Variable,
                    .Struct => .Struct,
                    .Enum => .Enum,
                    .Group => .Group,
                },
                .name = symbol_name,
                .original_module = module_path,
                .enum_role = if (symbol.kind == .Enum or symbol.kind == .Group) .Type else null,
                .enum_type_name = if (symbol.kind == .Enum or symbol.kind == .Group) symbol_name else null,
            });
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
                                    .enum_role = .Type,
                                    .enum_type_name = enum_decl.name.lexeme,
                                });
                                for (enum_decl.variants) |variant| {
                                    const variant_full_name = try std.fmt.allocPrint(self.allocator, "{s}.{s}", .{ symbol_name, variant.lexeme });
                                    try self.imported_symbols.?.put(variant_full_name, .{
                                        .kind = .Enum,
                                        .name = variant.lexeme,
                                        .original_module = module_path,
                                        .enum_role = .Variant,
                                        .enum_type_name = enum_decl.name.lexeme,
                                    });
                                }
                                return;
                            }
                        },
                        .GroupDecl => |group_decl| {
                            const is_public = group_decl.is_public;
                            if (is_public and std.mem.eql(u8, group_decl.name.lexeme, symbol_name)) {
                                try self.imported_symbols.?.put(symbol_name, .{
                                    .kind = .Group,
                                    .name = group_decl.name.lexeme,
                                    .original_module = module_path,
                                    .enum_role = .Type,
                                    .enum_type_name = group_decl.name.lexeme,
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
                        .ZigDecl, .Block, .MapDecl, .Return, .MapLiteral, .Module, .Import, .Path, .Continue, .Break, .Assert, .Cast => {},
                    }
                }
            },
            else => {},
        }
    }
};
