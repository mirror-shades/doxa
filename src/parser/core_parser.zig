const std = @import("std");
const token = @import("../types/token.zig");
const ast = @import("../ast/ast.zig");
const ModuleInfo = ast.ModuleInfo;
const Reporting = @import("../utils/reporting.zig");
const Reporter = Reporting.Reporter;
const Location = Reporting.Location;
const Errors = @import("../utils/errors.zig");
const ErrorList = Errors.ErrorList;
const ErrorCode = Errors.ErrorCode;
const import_parser = @import("import_parser.zig");
const declaration_parser = @import("declaration_parser.zig");
const statement_parser = @import("statement_parser.zig");
const expression_parser = @import("expression_parser.zig");
const internal_call_parser = @import("internal_call_parser.zig");
const module_resolver = @import("module_resolver.zig");
const Precedence = @import("./precedence.zig").Precedence;

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

    module_imports: std.StringHashMap(std.StringHashMap(@import("parser_types.zig").ModuleImportEntry)),

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
            .module_imports = std.StringHashMap(std.StringHashMap(@import("parser_types.zig").ModuleImportEntry)).init(allocator),
            .imported_symbols = std.StringHashMap(import_parser.ImportedSymbol).init(allocator),
            .declared_types = std.StringHashMap(void).init(allocator),
            .module_resolution_status = std.StringHashMap(ModuleResolutionStatus).init(allocator),
            .import_stack = std.array_list.Managed(ImportStackEntry).init(allocator),
        };

        return parser;
    }

    pub fn deinit(_: *Parser) void {}

    pub fn peek(self: *Parser) token.Token {
        var i = self.current;
        while (i < self.tokens.len - 1 and self.tokens[i].type == .SEMICOLON) {
            i += 1;
        }
        return self.tokens[i];
    }

    pub fn peekAhead(self: *Parser, offset: usize) token.Token {
        var i: usize = self.current;
        var remaining = offset;
        while (i < self.tokens.len - 1 and self.tokens[i].type == .SEMICOLON) {
            i += 1;
        }
        while (remaining > 0 and i < self.tokens.len - 1) {
            i += 1;
            if (self.tokens[i].type == .SEMICOLON) continue;
            remaining -= 1;
        }
        if (i >= self.tokens.len) {
            return self.tokens[self.tokens.len - 1];
        }
        while (i < self.tokens.len - 1 and self.tokens[i].type == .SEMICOLON) {
            i += 1;
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

    pub fn previous(self: *Parser) token.Token {
        if (self.current == 0) {
            return self.tokens[0];
        }
        return self.tokens[self.current - 1];
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

    fn reportWarning(self: *Parser, message: []const u8) void {
        _ = self;
        var reporting = Reporting.init();
        reporting.reportWarning("{s}", .{message});
    }

    pub fn execute(self: *Parser) ErrorList![]ast.Stmt {
        var statements = std.array_list.Managed(ast.Stmt).init(self.allocator);
        errdefer {
            for (statements.items) |*stmt| {
                stmt.deinit(self.allocator);
            }
            statements.deinit();
        }

        // Pass 1: Imports and Modules (Process side effects, don't add to statements list yet)
        var start_index_after_imports = self.current;
        while (self.tokens[start_index_after_imports].type == .IMPORT or self.tokens[start_index_after_imports].type == .MODULE) {
            const temp_current = self.current;
            self.current = start_index_after_imports;

            if (self.tokens[start_index_after_imports].type == .MODULE) {
                _ = try import_parser.parseModuleStmt(self, false);
            } else {
                _ = try import_parser.parseImportStmt(self, false);
            }

            start_index_after_imports = self.current;

            self.current = temp_current;
        }
        self.current = start_index_after_imports;

        // Pass 2: All other statements
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
                .FUNCTION => {
                    var func = try declaration_parser.parseFunctionDecl(self);
                    func.data.FunctionDecl.is_public = is_public;
                    func.data.FunctionDecl.is_entry = is_entry;
                    if (is_entry) {
                        if (self.entry_point_location != null) {
                            self.entry_point_name = func.data.FunctionDecl.name.lexeme;
                        } else {
                            return error.MultipleEntryPoints;
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
                    _ = try import_parser.parseImportStmt(self, false);
                },
                .MODULE => {
                    if (is_entry) {
                        return error.MisplacedEntryPoint;
                    }
                    if (is_public) {
                        return error.MisplacedPublicModifier;
                    }
                    _ = try import_parser.parseModuleStmt(self, false);
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

    pub fn loadModuleSourceWithPath(self: *Parser, module_path: []const u8) ![]const u8 {
        return module_resolver.loadModuleSourceWithPath(self, module_path);
    }

    pub fn loadModuleSource(self: *Parser, module_path: []const u8) ![]const u8 {
        return module_resolver.loadModuleSource(self, module_path);
    }

    pub fn extractModuleInfo(self: *Parser, source: []const u8, file_path: []const u8) !ModuleInfo {
        _ = source;
        // TODO: Implement this properly
        return ModuleInfo{
            .file_path = file_path,
            .public_symbols = std.array_list.Managed(ast.ModuleSymbol).init(self.allocator),
            .imports = std.array_list.Managed(ast.ImportStmt).init(self.allocator),
        };
    }

    pub fn extractModuleInfoWithParser(self: *Parser, source: []const u8, file_path: []const u8, module_name: ?[]const u8, parser: *Parser) !ModuleInfo {
        _ = source;
        _ = module_name;
        _ = parser;
        // TODO: Implement this properly
        return ModuleInfo{
            .file_path = file_path,
            .public_symbols = std.array_list.Managed(ast.ModuleSymbol).init(self.allocator),
            .imports = std.array_list.Managed(ast.ImportStmt).init(self.allocator),
        };
    }

    pub fn input(self: *Parser, left: ?*ast.Expr, _: Precedence) ErrorList!?*ast.Expr {
        _ = left;

        const input_token = self.peek();
        self.advance();

        const empty_prompt = token.Token{
            .file = self.current_file,
            .file_uri = self.current_file_uri,
            .type = .STRING,
            .lexeme = "",
            .literal = .{ .string = "" },
            .line = input_token.line,
            .column = input_token.column,
        };

        const input_expr = try self.allocator.create(ast.Expr);
        input_expr.* = .{
            .base = .{
                .id = ast.generateNodeId(),
                .span = ast.SourceSpan.fromToken(input_token),
            },
            .data = .{
                .Input = .{
                    .prompt = empty_prompt,
                },
            },
        };

        return input_expr;
    }

    pub fn internalCallExpr(self: *Parser, left: ?*ast.Expr, precedence: Precedence) ErrorList!?*ast.Expr {
        return internal_call_parser.internalCallExpr(self, left, precedence);
    }

    pub fn parsePrintMethod(self: *Parser) ErrorList!?*ast.Expr {
        return internal_call_parser.parsePrintMethod(self);
    }

    pub fn index(self: *Parser, left: ?*ast.Expr, _: Precedence) ErrorList!?*ast.Expr {
        if (left == null) return error.ExpectedLeftOperand;

        const index_expr = try expression_parser.parseExpression(self) orelse return error.ExpectedExpression;

        if (self.peek().type != .RIGHT_BRACKET) return error.ExpectedRightBracket;
        self.advance();

        const index_node = try self.allocator.create(ast.Expr);
        index_node.* = .{
            .base = .{
                .id = ast.generateNodeId(),
                .span = ast.SourceSpan.fromToken(self.previous()),
            },
            .data = .{
                .Index = .{
                    .array = left.?,
                    .index = index_expr,
                },
            },
        };

        return index_node;
    }

    pub fn assignment(self: *Parser, left: ?*ast.Expr, _: Precedence) ErrorList!?*ast.Expr {
        if (left == null) return error.ExpectedLeftOperand;

        const right = try expression_parser.parseExpression(self) orelse return error.ExpectedExpression;

        const assignment_node = try self.allocator.create(ast.Expr);
        assignment_node.* = .{
            .base = .{
                .id = ast.generateNodeId(),
                .span = ast.SourceSpan.fromToken(self.previous()),
            },
            .data = .{
                .Assignment = .{
                    .name = self.previous(),
                    .value = right,
                },
            },
        };

        return assignment_node;
    }

    pub fn fieldAccess(self: *Parser, left: ?*ast.Expr, _: Precedence) ErrorList!?*ast.Expr {
        if (left == null) return error.ExpectedLeftOperand;
        if (self.peek().type != .IDENTIFIER) return error.ExpectedIdentifier;
        const field_name = self.peek();
        self.advance();

        const field_access_node = try self.allocator.create(ast.Expr);
        field_access_node.* = .{
            .base = .{
                .id = ast.generateNodeId(),
                .span = ast.SourceSpan.fromToken(field_name),
            },
            .data = .{
                .FieldAccess = .{
                    .object = left.?,
                    .field = field_name,
                },
            },
        };

        return field_access_node;
    }

    pub fn enumMember(self: *Parser, left: ?*ast.Expr, _: Precedence) ErrorList!?*ast.Expr {
        if (left == null) return error.ExpectedLeftOperand;
        if (self.peek().type != .IDENTIFIER) return error.ExpectedIdentifier;
        const member_name = self.peek();
        self.advance();

        const enum_member_node = try self.allocator.create(ast.Expr);
        enum_member_node.* = .{
            .base = .{
                .id = ast.generateNodeId(),
                .span = ast.SourceSpan.fromToken(member_name),
            },
            .data = .{
                .EnumMember = member_name,
            },
        };

        return enum_member_node;
    }
};
