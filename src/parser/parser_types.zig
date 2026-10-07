const std = @import("std");
const token = @import("../types/token.zig");
const declaration_parser = @import("declaration_parser.zig");
const expression_parser = @import("expression_parser.zig");
const statement_parser = @import("statement_parser.zig");
const Precedence = @import("./precedence.zig").Precedence;
const precedence = @import("./precedence.zig");
const LexicalAnalyzer = @import("../analysis/lexical.zig").LexicalAnalyzer;
const import_parser = @import("import_parser.zig");
const internal_call_parser = @import("internal_call_parser.zig");

const ast = @import("../ast/ast.zig");

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

fn parserErrorHint(err: anyerror) []const u8 {
    return switch (err) {
        error.ExpectedComma => "expected a ',' between arguments or list elements",
        error.ExpectedCommaOrBrace => "expected ',' or '}'",
        error.ExpectedCommaOrParen => "expected ',' or ')'",
        error.ExpectedCommaOrBracket => "expected ',' or ']'",
        error.ExpectedCommaOrClosingBracket => "expected ',' or a closing bracket",
        error.ExpectedCommaOrClosingParenthesis => "expected ',' or a closing parenthesis",
        error.ExpectedRightParen, error.ExpectedClosingParen, error.ExpectedClosingParenthesis => "expected a closing parenthesis ')'",
        error.ExpectedLeftParen => "expected an opening parenthesis '('",
        error.ExpectedRightBrace => "expected a closing brace '}'",
        error.ExpectedLeftBrace => "expected an opening brace '{'",
        error.ExpectedRightBracket => "expected a closing bracket ']'",
        error.ExpectedLeftBracket => "expected an opening bracket '['",
        error.ExpectedExpression => "expected an expression",
        error.ExpectedIdentifier => "expected an identifier",
        error.ExpectedType => "expected a type",
        error.ExpectedThen => "expected 'then' after the condition",
        error.ExpectedElse => "expected 'else'",
        error.ExpectedColon => "expected ':'",
        error.ExpectedAssignmentOperator => "expected an assignment operator",
        error.ExpectedReturnsKeyword => "expected 'returns'",
        error.ExpectedLeftBraceOrReturnsKeyword => "expected '{' or 'returns'",
        error.ExpectedFunctionName => "expected a function name",
        error.ExpectedFunctionParams => "expected function parameters",
        error.ExpectedFunctionBody => "expected a function body",
        error.ExpectedFunctionReturnType => "expected a function return type",
        error.ExpectedString, error.ExpectedStringLiteral => "expected a string literal",
        error.ExpectedMapKey => "expected a map key",
        error.ExpectedInKeyword => "expected 'in'",
        error.ExpectedWhereKeyword => "expected 'where'",
        error.ExpectedMapKeyword => "expected 'map'",
        error.ExpectedPattern => "expected a pattern",
        error.ExpectedEnumVariant => "expected an enum variant",
        error.ExpectedModuleName => "expected a module name",
        error.ExpectedImportName => "expected an import name",
        error.UnexpectedToken => "unexpected token",
        error.ParserDidNotAdvance => "parser could not make progress",
        error.InternalParserError => "internal parser error",
        else => "",
    };
}

fn isPrintableAscii(text: []const u8) bool {
    for (text) |byte| {
        if (byte < 0x20 or byte > 0x7e) return false;
    }
    return true;
}

pub const Parser = struct {
    tokens: []const token.Token,
    current: usize,
    io: std.Io,
    allocator: std.mem.Allocator,
    reporter: *Reporter,

    has_entry_point: bool = false,
    entry_point_location: ?token.Token = null,
    entry_point_name: ?[]const u8 = null,

    current_file: []const u8,
    current_file_uri: []const u8,

    // Match path pattern tracking
    current_path_pattern_tokens: ?[]const token.Token = null,
    current_path_pattern_is_wildcard: bool = false,
    current_path_pattern_field_names: ?[]const token.Token = null,

    /// A parser is pure syntax: it turns one file's tokens into statements and
    /// knows nothing of other files. `module` and `import` statements stay in
    /// the statement list; the module loader binds them when it collects the
    /// file's declarations.
    pub fn init(io: std.Io, allocator: std.mem.Allocator, tokens: []const token.Token, current_file: []const u8, current_file_uri: []const u8, reporter: *Reporter) Parser {
        return Parser{
            .io = io,
            .allocator = allocator,
            .tokens = tokens,
            .current = 0,
            .reporter = reporter,
            .current_file = current_file,
            .current_file_uri = current_file_uri,
        };
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
        return self.parseBlockBody(true);
    }

    pub fn liftBlock(self: *Parser) ErrorList!?*ast.Expr {
        return self.parseBlockBody(false);
    }

    /// The three forms a branch body may take, reported so a caller can apply
    /// its own terminator rules to the ones that need them.
    pub const BranchForm = enum {
        /// A bare `return`/`break`/`continue`.
        diverging,
        /// A braced `{ ... }` body.
        block,
        /// A trailing expression.
        expression,
    };

    pub const BranchBody = struct {
        /// `match` arms keep a block's implicit last-expression return; `if`
        /// and `as` branches require an explicit `lift`.
        allow_implicit_value: bool,
        /// The `else` of an `if` reads its expression at the lowest precedence
        /// level; every other slot reads a full expression.
        lowest_precedence: bool,
    };

    /// Parses the body of a `then`/`else` branch or a `match` arm.
    ///
    /// A bare `return`/`break`/`continue` is wrapped into the single-statement
    /// block it abbreviates, so the rest of the compiler only ever sees one
    /// shape and divergence analysis is unchanged by the surface syntax.
    pub fn branchBody(self: *Parser, opts: BranchBody, form: *BranchForm) ErrorList!?*ast.Expr {
        switch (self.peek().type) {
            .CONTINUE, .BREAK, .RETURN => {
                const stmt: ast.Stmt = switch (self.peek().type) {
                    .CONTINUE => try statement_parser.parseContinueStmt(self),
                    .BREAK => try statement_parser.parseBreakStmt(self),
                    .RETURN => try statement_parser.parseReturnStmt(self),
                    else => unreachable,
                };

                const statements = try self.allocator.alloc(ast.Stmt, 1);
                statements[0] = stmt;

                const block_expr = try self.allocator.create(ast.Expr);
                block_expr.* = .{
                    .base = .{
                        .id = ast.generateNodeId(),
                        .span = ast.SourceSpan.fromToken(self.previous()),
                    },
                    .data = .{ .Block = .{ .statements = statements, .value = null } },
                };

                form.* = .diverging;
                return block_expr;
            },
            .LEFT_BRACE => {
                form.* = .block;
                return self.parseBlockBody(opts.allow_implicit_value);
            },
            else => {
                form.* = .expression;
                if (opts.lowest_precedence) {
                    return precedence.parsePrecedence(self, .NONE);
                }
                return expression_parser.parseExpression(self);
            },
        }
    }

    fn parseBlockBody(self: *Parser, allow_implicit_value: bool) ErrorList!?*ast.Expr {
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

        var block_value: ?*ast.Expr = null;

        while (self.peek().type != .RIGHT_BRACE and self.peek().type != .EOF) {
            const stmt = try statement_parser.parseStatement(self);

            if (stmt.data == .Lift) {
                block_value = stmt.data.Lift.value;
                break;
            }

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

        if (allow_implicit_value and block_value == null and statements.items.len > 0) {
            const last_idx = statements.items.len - 1;
            const last_stmt = statements.items[last_idx];
            switch (last_stmt.data) {
                .Expression => |maybe_expr| {
                    if (maybe_expr) |e| {
                        block_value = e;
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
                    .value = block_value,
                },
            },
        };

        return block_expr;
    }

    /// Report a parser-phase failure at the token the parser stopped on.
    /// `execute` returns errors without emitting a diagnostic — the caller owns
    /// presentation — so every caller must route through here: the root file in
    /// `main.zig` and each lazily parsed imported module in `module/loader.zig`.
    /// Otherwise a parse failure in an imported module surfaces as an unlocated
    /// internal compiler error instead of a syntax error at the offending line.
    pub fn reportParseError(self: *Parser, err: anyerror) void {
        const tok = self.peek();

        const file = if (tok.file.len > 0) tok.file else self.current_file;
        const file_uri = if (tok.file_uri.len > 0) tok.file_uri else self.current_file_uri;

        const loc = Location{
            .file = file,
            .file_uri = file_uri,
            .range = .{
                .start_line = tok.line,
                .start_col = tok.column,
                .end_line = tok.line,
                .end_col = tok.column + tok.lexeme.len,
            },
        };

        var lexeme_buf: [64]u8 = undefined;
        var token_desc: []const u8 = @tagName(tok.type);
        if (tok.lexeme.len > 0 and tok.lexeme.len <= lexeme_buf.len and isPrintableAscii(tok.lexeme)) {
            token_desc = std.fmt.bufPrint(&lexeme_buf, "{s} '{s}'", .{ @tagName(tok.type), tok.lexeme }) catch @tagName(tok.type);
        }

        const hint = parserErrorHint(err);
        if (hint.len > 0) {
            self.reporter.reportCompileError(
                loc,
                ErrorCode.SYNTAX_ERROR,
                "{s}: {s}; found {s}",
                .{ @errorName(err), hint, token_desc },
            );
        } else {
            self.reporter.reportCompileError(
                loc,
                ErrorCode.SYNTAX_ERROR,
                "parse error: {s}; found {s}",
                .{ @errorName(err), token_desc },
            );
        }
    }

    pub fn execute(self: *Parser) ErrorList![]ast.Stmt {
        var statements = std.array_list.Managed(ast.Stmt).init(self.allocator);
        errdefer {
            for (statements.items) |*stmt| {
                stmt.deinit(self.allocator);
            }
            statements.deinit();
        }

        // parse all statements (single pass)
        while (self.peek().type != .EOF) {
            const loop_start_pos = self.current;

            var is_public = false;
            var is_entry = false;

            if (self.peek().type == .PUBLIC) {
                is_public = true;
                self.advance();
            }
            if (self.peek().type == .ENTRY) {
                is_entry = true;
                const entry_token = self.peek();
                self.advance();
                if (!self.has_entry_point) {
                    self.has_entry_point = true;
                    self.entry_point_location = entry_token;
                }
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
                .MAP_KEYWORD => {
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
                    try statements.append(try import_parser.parseImportStmt(self, is_public));
                },
                .MODULE => {
                    if (is_entry) {
                        return error.MisplacedEntryPoint;
                    }
                    try statements.append(try import_parser.parseModuleStmt(self, is_public));
                },
                .STRUCT_KEYWORD => {
                    const expr = try declaration_parser.parseStructDecl(self, null, .NONE);
                    if (expr) |non_null_expr| {
                        switch (non_null_expr.data) {
                            .StructDecl => |*struct_decl| struct_decl.is_public = is_public,
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
                .ENUM_KEYWORD => {
                    var enum_decl = try declaration_parser.parseEnumDecl(self);
                    enum_decl.data.EnumDecl.is_public = is_public;
                    if (is_entry) {
                        return error.InvalidEntryPoint;
                    }
                    try statements.append(enum_decl);
                },
                .GROUP_KEYWORD => {
                    var group_decl = try declaration_parser.parseGroupDecl(self);
                    group_decl.data.GroupDecl.is_public = is_public;
                    if (is_entry) {
                        return error.InvalidEntryPoint;
                    }
                    try statements.append(group_decl);
                },
                .IF, .WHILE, .RETURN, .LEFT_BRACE, .EACH, .DEFER => {
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
            // A module-qualified type (`ns.Type{...}`) keeps its full spelling;
            // semantic analysis resolves it through the file's bindings.
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

        // `value.@push(1)` is `@push(value, 1)`.
        if (internal_call_parser.isPostfixIntrinsic(current_token.type)) {
            return try internal_call_parser.postfixInternalCall(self, left.?);
        }
        if (current_token.type != .IDENTIFIER) {
            return error.ExpectedIdentifier;
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
            return try self.call(field_access, .CALL);
        }

        return field_access;
    }

    pub fn internalCallExpr(self: *Parser, left: ?*ast.Expr, prec: Precedence) ErrorList!?*ast.Expr {
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

    fn reportWarning(self: *Parser, message: []const u8) void {
        _ = self;
        var reporting = Reporting.init();
        reporting.reportWarning("{s}", .{message});
    }

    fn check(self: *Parser, token_type: token.TokenType) bool {
        return self.peek().type == token_type;
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
};
