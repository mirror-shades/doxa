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

//======================================================================

const TokenStyle = enum {
    Keyword,
    Symbol,
    Undefined,
};

// Add module resolution status tracking
pub const ModuleResolutionStatus = enum {
    NOT_STARTED,
    IN_PROGRESS,
    COMPLETED,
};

// Add import stack entry for tracking import chains
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
    current_module: ?ModuleInfo = null,
    module_cache: std.StringHashMap(ModuleInfo),
    module_namespaces: std.StringHashMap(ModuleInfo),

    module_imports: std.StringHashMap(std.StringHashMap([]const u8)),

    imported_symbols: ?std.StringHashMap(import_parser.ImportedSymbol) = null,

    declared_types: std.StringHashMap(void),

    // Add circular import detection fields
    module_resolution_status: std.StringHashMap(ModuleResolutionStatus),
    import_stack: std.ArrayList(ImportStackEntry),

    pub fn init(allocator: std.mem.Allocator, tokens: []const token.Token, current_file: []const u8, reporter: *Reporter) Parser {
        const parser = Parser{
            .allocator = allocator,
            .tokens = tokens,
            .current = 0,
            .reporter = reporter,
            .current_file = current_file,
            .module_cache = std.StringHashMap(ModuleInfo).init(allocator),
            .module_namespaces = std.StringHashMap(ModuleInfo).init(allocator),
            .module_imports = std.StringHashMap(std.StringHashMap([]const u8)).init(allocator),
            .imported_symbols = std.StringHashMap(import_parser.ImportedSymbol).init(allocator),
            .declared_types = std.StringHashMap(void).init(allocator),
            .module_resolution_status = std.StringHashMap(ModuleResolutionStatus).init(allocator),
            .import_stack = std.ArrayList(ImportStackEntry).init(allocator),
        };

        return parser;
    }

    pub fn deinit(_: *Parser) void {}

    pub fn peek(self: *Parser) token.Token {
        var i = self.current;
        // Skip over any semicolon tokens (non-semantic separators)
        while (i < self.tokens.len - 1 and self.tokens[i].type == .SEMICOLON) {
            i += 1;
        }
        return self.tokens[i];
    }

    pub fn peekAhead(self: *Parser, offset: usize) token.Token {
        // Walk forward over tokens, skipping semicolons and counting only non-semicolons
        var i: usize = self.current;
        var remaining = offset;
        // Ensure we start from a non-semicolon for offset 0 as well
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
        // If we landed on a semicolon, skip to next non-semicolon or EOF
        while (i < self.tokens.len - 1 and self.tokens[i].type == .SEMICOLON) {
            i += 1;
        }
        return self.tokens[i];
    }

    pub fn advance(self: *Parser) void {
        // Move to the next non-semicolon token
        if (self.current < self.tokens.len - 1) {
            self.current += 1;
            while (self.current < self.tokens.len - 1 and self.tokens[self.current].type == .SEMICOLON) {
                self.current += 1;
            }
        }
    }

    pub fn block(self: *Parser, _: ?*ast.Expr, _: Precedence) ErrorList!?*ast.Expr {

        // Consume {
        if (self.peek().type != .LEFT_BRACE) {
            return error.ExpectedLeftBrace;
        }
        self.advance();

        var statements = std.ArrayList(ast.Stmt).init(self.allocator);
        errdefer {
            for (statements.items) |*stmt| {
                stmt.deinit(self.allocator);
            }
            statements.deinit();
        }

        var last_expr: ?*ast.Expr = null;

        while (self.peek().type != .RIGHT_BRACE and self.peek().type != .EOF) {
            // Parse any statement type
            const stmt = try statement_parser.parseStatement(self);
            try statements.append(stmt);

            // Break after return statement
            if (stmt.data == .Return) break;

            if (self.peek().type == .RIGHT_BRACE) {
                break;
            }
        }

        if (self.peek().type != .RIGHT_BRACE) {
            return error.ExpectedRightBrace;
        }
        self.advance();

        // Determine the last expression in the block (if any) ONLY if it is the
        // final statement. Do not skip control-flow statements that follow.
        if (statements.items.len > 0) {
            const last_idx = statements.items.len - 1;
            const last_stmt = statements.items[last_idx];
            switch (last_stmt.data) {
                .Expression => |maybe_expr| {
                    if (maybe_expr) |e| {
                        last_expr = e;
                    }
                    // Remove only the final expression statement
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
        var statements = std.ArrayList(ast.Stmt).init(self.allocator);
        errdefer {
            for (statements.items) |*stmt| {
                stmt.deinit(self.allocator);
            }
            statements.deinit();
        }

        // --- Pass 1: Imports and Modules (Process side effects, don't add to statements list yet) ---
        // Keep track of the original position after imports are processed
        var start_index_after_imports = self.current;
        while (self.tokens[start_index_after_imports].type == .IMPORT or self.tokens[start_index_after_imports].type == .MODULE) {
            // Temporarily move parser to process the import/module
            const temp_current = self.current;
            self.current = start_index_after_imports;

            // Call appropriate parser based on token type
            if (self.tokens[start_index_after_imports].type == .MODULE) {
                _ = try import_parser.parseModuleStmt(self); // Call for side-effects (loading/registration)
            } else {
                _ = try import_parser.parseImportStmt(self); // Call for side-effects (loading/registration)
            }

            // Advance start_index_after_imports past the import/module statement
            // parseImportStmt/parseModuleStmt should have advanced self.current appropriately
            start_index_after_imports = self.current;

            // Restore original parser position if needed (though we'll reset below)
            self.current = temp_current;
        }
        // Reset parser to position after imports for the main parsing pass
        self.current = start_index_after_imports;

        // --- Pass 2: All other statements ---
        while (self.peek().type != .EOF) {
            const loop_start_pos = self.current; // Track position for progress check

            // --- Handle Modifiers ---
            var is_public = false;
            var is_entry = false;

            // IMPORTANT: Check for entry first as it might appear before PUBLIC
            if (self.peek().type == .ENTRY) {
                is_entry = true;
                const entry_token = self.peek(); // Store token for later use if needed
                self.advance(); // consume entry
                // Store entry point location immediately if found
                if (!self.has_entry_point) { // Only store the first one found
                    self.has_entry_point = true;
                    self.entry_point_location = entry_token;
                }
            }
            if (self.peek().type == .PUBLIC) {
                is_public = true;
                self.advance(); // consume pub
            }
            // --- End Handle Modifiers ---

            // --- Parse Statement ---
            // Use a general statement parser that handles declarations, expressions, control flow etc.
            // Pass the detected modifiers to the statement parser if necessary,
            // or apply them to the result afterwards.

            // Use a specific parser based on the *next* token (after modifiers)
            const stmt_token_type = self.peek().type;
            switch (stmt_token_type) {
                .VAR, .CONST => {
                    var decl = try declaration_parser.parseVarDecl(self);
                    decl.data.VarDecl.is_public = is_public; // Apply modifier
                    // Cannot be entry point
                    if (is_entry) {
                        return error.InvalidEntryPoint;
                    }
                    try statements.append(decl);
                },
                .FUNCTION => {
                    var func = try declaration_parser.parseFunctionDecl(self);
                    func.data.FunctionDecl.is_public = is_public; // Apply modifiers
                    func.data.FunctionDecl.is_entry = is_entry;
                    if (is_entry) {
                        // Store entry point name if this function is the entry point
                        // Check if location was already set by '->' token earlier
                        if (self.entry_point_location != null) {
                            self.entry_point_name = func.data.FunctionDecl.name.lexeme;
                        } else {
                            // This case means func keyword without preceding '->', shouldn't happen if logic is correct
                            {
                                return error.MultipleEntryPoints;
                            }
                        }
                    }
                    try statements.append(func);
                },
                // Allow import/module statements anywhere (not only at top)
                .IMPORT => {
                    // Modifiers invalid with import
                    if (is_entry) {
                        return error.MisplacedEntryPoint;
                    }
                    if (is_public) {
                        return error.MisplacedPublicModifier;
                    }
                    // Perform side-effects (load/register) and do not append as a normal statement
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
                    // Apply is_public to struct decl in AST
                    if (expr) |non_null_expr| {
                        switch (non_null_expr.data) {
                            .StructDecl => |*struct_decl| {
                                struct_decl.is_public = is_public;
                            },
                            else => {},
                        }
                    }
                    // Cannot be entry point
                    if (is_entry) {
                        return error.InvalidEntryPoint;
                    }
                    // Append only if expr is not null
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
                    enum_decl.data.EnumDecl.is_public = is_public; // Apply modifier
                    // Cannot be entry point
                    if (is_entry) {
                        return error.InvalidEntryPoint;
                    }
                    try statements.append(enum_decl);
                },
                // Handle other statement types recognised by statement_parser
                .IF, .WHILE, .RETURN, .LEFT_BRACE, .EACH => {
                    // Modifiers likely invalid here
                    if (is_entry) {
                        return error.MisplacedEntryPoint;
                    }
                    if (is_public) {
                        return error.MisplacedPublicModifier;
                    }
                    const parsed_stmt = try statement_parser.parseStatement(self);
                    // Only append if it's not a null expression (e.g. from empty block)
                    if (!(parsed_stmt.data == .Expression and parsed_stmt.data.Expression == null)) {
                        try statements.append(parsed_stmt);
                    }
                },
                .ASSERT => { // Add ASSERT case
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
                // Default to expression statement
                else => {
                    // Modifiers likely invalid here unless it's a top-level expression
                    // that somehow should be public/entry? Revisit if needed.
                    if (is_entry) {
                        return error.MisplacedEntryPoint;
                    }
                    if (is_public) {
                        return error.MisplacedPublicModifier;
                    }
                    const expr_stmt = try statement_parser.parseExpressionStmt(self);
                    // Only append if it's not a null expression
                    if (!(expr_stmt.data == .Expression and expr_stmt.data.Expression == null)) {
                        try statements.append(expr_stmt);
                    }
                },
            }

            // --- Progress Check ---
            // Ensure the parser advanced. If not, it means a sub-parser failed to consume tokens.
            if (self.current == loop_start_pos and self.peek().type != .EOF) {
                // Log error, force advance, or return error? Returning error is safest.
                return error.ParserDidNotAdvance;
            }
        }

        // Final check: If entry point marker '->' was found but no function followed
        if (self.has_entry_point and self.entry_point_name == null) {
            // Report error using self.entry_point_location
            const loc = Location{
                .file = self.current_file,
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
        // Find the opening brace of the function body
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
                    // Check next token
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
        self.advance(); // consume (

        var arguments = std.ArrayList(ast.CallArgument).init(self.allocator);
        errdefer {
            for (arguments.items) |arg| {
                arg.expr.deinit(self.allocator);
                self.allocator.destroy(arg.expr);
            }
            arguments.deinit();
        }

        // Handle empty arglist as a degenerate case; still allow sugar rewrite
        if (self.peek().type == .RIGHT_PAREN) {
            // No args; fall through to common close+build logic
        } else {
            // Parse arguments
            while (true) {
                var arg_expr: *ast.Expr = undefined;
                var is_alias = false;
                if (self.peek().type == .TILDE) {
                    // Handle default argument placeholder
                    self.advance(); // consume ~
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
                    self.advance(); // consume ^
                    // Parse the argument after ^
                    arg_expr = try expression_parser.parseExpression(self) orelse return error.ExpectedExpression;
                    is_alias = true;
                } else {
                    arg_expr = try expression_parser.parseExpression(self) orelse return error.ExpectedExpression;
                }
                try arguments.append(.{ .expr = arg_expr, .is_alias = is_alias });

                if (self.peek().type == .RIGHT_PAREN) break;
                if (self.peek().type != .COMMA) return error.ExpectedComma;
                self.advance(); // consume comma
            }
        }
        // Expect closing parenthesis
        if (self.peek().type != .RIGHT_PAREN) {
            return error.ExpectedRightParen;
        }
        self.advance(); // consume )

        // Disallow dot-syntax method sugar to avoid ambiguity with user fields/methods
        if (callee.?.data == .FieldAccess) {
            const fa = callee.?.data.FieldAccess;
            if (Parser.methodNameToTokenType(fa.field.lexeme)) |_| {
                const loc: Location = .{ .file = self.current_file, .range = .{ .start_line = fa.field.line, .start_col = fa.field.column, .end_line = fa.field.line, .end_col = fa.field.column } };
                self.reporter.reportCompileError(loc, ErrorCode.UNKNOWN_METHOD, "Unknown field or method '{s}'. If this is a compiler method, use @{s}(...)", .{ fa.field.lexeme, fa.field.lexeme });
                return error.UnknownFieldOrMethod;
            }
        }

        // Fallback: regular function call
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
        if (std.mem.eql(u8, name, "clear")) return .CLEAR;

        // Search/index
        if (std.mem.eql(u8, name, "find")) return .FIND;

        // Type conversions
        if (std.mem.eql(u8, name, "string")) return .TOSTRING;
        if (std.mem.eql(u8, name, "int")) return .TOINT;
        if (std.mem.eql(u8, name, "float")) return .TOFLOAT;
        if (std.mem.eql(u8, name, "byte")) return .TOBYTE;

        // I/O
        if (std.mem.eql(u8, name, "read")) return .READ;
        if (std.mem.eql(u8, name, "write")) return .WRITE;
        if (std.mem.eql(u8, name, "syscall")) return .SYSCALL;

        // Control flow
        if (std.mem.eql(u8, name, "panic")) return .PANIC;
        if (std.mem.eql(u8, name, "assert")) return .ASSERT;

        // Copy/clone
        if (std.mem.eql(u8, name, "shallow")) return .SHALLOW;

        return null;
    }

    pub fn parseStructInit(self: *Parser) ErrorList!?*ast.Expr {
        const struct_name = self.peek();
        const start_pos = self.current;
        self.advance();

        // Handle namespace.Symbol struct access
        if (self.peek().type == .DOT) {
            // This could be a namespace.Struct initialization
            const namespace = struct_name.lexeme;

            // Check if namespace exists
            if (self.module_namespaces.contains(namespace)) {
                self.advance(); // consume dot

                // Get the struct type name
                if (self.peek().type != .IDENTIFIER) {
                    self.current = start_pos; // Reset position
                    return null;
                }

                const type_name = self.peek();
                self.advance();

                // Check for opening brace
                if (self.peek().type != .LEFT_BRACE) {
                    self.current = start_pos; // Reset position
                    return null;
                }

                // Parse struct initialization
                self.advance(); // consume {

                var fields = std.ArrayList(*ast.StructInstanceField).init(self.allocator);
                errdefer {
                    for (fields.items) |field| {
                        field.deinit(self.allocator);
                        self.allocator.destroy(field);
                    }
                    fields.deinit();
                }

                while (self.peek().type != .RIGHT_BRACE) {
                    // Allow blank lines between fields
                    while (self.peek().type == .NEWLINE) self.advance();

                    // Parse field name
                    if (self.peek().type != .IDENTIFIER) {
                        return error.ExpectedIdentifier;
                    }
                    const field_name = self.peek();
                    self.advance();

                    // Expect equals
                    if (self.peek().type != .ASSIGN) {
                        return error.ExpectedEquals;
                    }
                    self.advance();

                    // Parse field value - try struct init first, then fall back to regular expression
                    var value: *ast.Expr = undefined;
                    if (self.peek().type == .IDENTIFIER) {
                        if (try parseStructInit(self)) |struct_init| {
                            value = struct_init;
                        } else {
                            value = try expression_parser.parseExpression(self) orelse return error.ExpectedExpression;
                        }
                    } else {
                        value = try expression_parser.parseExpression(self) orelse return error.ExpectedExpression;
                    }

                    // Create field
                    const field = try self.allocator.create(ast.StructInstanceField);
                    field.* = .{
                        .name = field_name,
                        .value = value,
                    };
                    try fields.append(field);

                    // Handle separators: comma and/or newline(s)
                    if (self.peek().type == .COMMA) {
                        self.advance();
                    }
                    while (self.peek().type == .NEWLINE) self.advance();
                    // Allow trailing comma/newlines before closing brace
                }

                self.advance(); // consume }

                // Create struct literal
                const struct_init = try self.allocator.create(ast.Expr);
                struct_init.* = .{
                    .base = .{
                        .id = ast.generateNodeId(),
                        .span = ast.SourceSpan.fromToken(self.peek()),
                    },
                    .data = .{
                        .StructLiteral = .{
                            .name = type_name,
                            .fields = try fields.toOwnedSlice(),
                        },
                    },
                };
                return struct_init;
            }
        }

        if (self.peek().type != .LEFT_BRACE) {
            self.current = start_pos;
            return null;
        }
        self.advance(); // consume {

        var fields = std.ArrayList(*ast.StructInstanceField).init(self.allocator);
        errdefer {
            for (fields.items) |field| {
                field.deinit(self.allocator);
                self.allocator.destroy(field);
            }
            fields.deinit();
        }

        while (self.peek().type != .RIGHT_BRACE) {
            // Allow blank lines between fields
            while (self.peek().type == .NEWLINE) self.advance();

            // Parse field name
            if (self.peek().type != .IDENTIFIER) {
                return error.ExpectedIdentifier;
            }
            const field_name = self.peek();
            self.advance();

            // Expect equals
            if (self.peek().type != .ASSIGN) {
                return error.ExpectedEquals;
            }
            self.advance();

            // Parse field value - try struct init first, then fall back to regular expression
            var value: *ast.Expr = undefined;
            if (self.peek().type == .IDENTIFIER) {
                if (try parseStructInit(self)) |struct_init| {
                    value = struct_init;
                } else {
                    value = try expression_parser.parseExpression(self) orelse return error.ExpectedExpression;
                }
            } else {
                value = try expression_parser.parseExpression(self) orelse return error.ExpectedExpression;
            }

            // Create field
            const field = try self.allocator.create(ast.StructInstanceField);
            field.* = .{
                .name = field_name,
                .value = value,
            };
            try fields.append(field);

            // Handle separators: comma and/or newline(s)
            if (self.peek().type == .COMMA) {
                self.advance();
            }
            while (self.peek().type == .NEWLINE) self.advance();
            // Allow trailing comma/newlines before closing brace
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
            self.advance(); // consume [
        }

        // Parse the index expression
        const index_expr = try expression_parser.parseExpression(self) orelse return error.ExpectedExpression;

        // Check for and consume the RIGHT_BRACKET
        if (self.peek().type != .RIGHT_BRACKET) {
            index_expr.deinit(self.allocator);
            self.allocator.destroy(index_expr);
            return error.ExpectedRightBracket;
        }
        self.advance(); // consume ]

        // Check if this is an assignment
        if (self.peek().type == .ASSIGN) {
            self.advance(); // consume is
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

        // Check for another index operation (for nested access)
        if (self.peek().type == .LEFT_BRACKET) {
            self.advance(); // consume [
            return self.index(expr, .NONE);
        }

        return expr;
    }

    pub fn assignment(self: *Parser, left: ?*ast.Expr, _: Precedence) ErrorList!?*ast.Expr {
        if (left == null) return error.ExpectedExpression;

        // Add check for equals sign
        if (self.previous().type != .ASSIGN) {
            return error.UseIsForAssignment;
        }

        // Handle other expressions
        const value = try expression_parser.parseExpression(self) orelse return error.ExpectedExpression;

        // Check if left side is a valid assignment target
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

        // If we're still positioned at '.', consume it; in many Pratt paths the dot
        // is already consumed before calling this function.
        if (self.peek().type == .DOT) {
            self.advance(); // consume '.'
        }

        // Store current position and token (identifier after '.')
        const current_token = self.peek();

        // Check if this is a method name - if so, error early
        if (Parser.methodNameToTokenType(current_token.lexeme)) |_| {
            const loc: Location = .{ .file = self.current_file, .range = .{ .start_line = current_token.line, .start_col = current_token.column, .end_line = current_token.line, .end_col = current_token.column } };
            self.reporter.reportCompileError(loc, ErrorCode.UNKNOWN_METHOD, "Unknown field or method '{s}'. If this is a compiler method, use @{s}(...)", .{ current_token.lexeme, current_token.lexeme });
            return error.UnknownFieldOrMethod;
        }

        // Expect identifier or field access token
        if (current_token.type != .IDENTIFIER and current_token.type != .FIELD_ACCESS) {
            return error.ExpectedIdentifier;
        }

        self.advance(); // consume identifier

        // Create field access expression
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

        // Handle array indexing if present
        if (self.peek().type == .LEFT_BRACKET) {
            self.advance(); // consume [
            return try self.index(field_access, .NONE);
        }

        return field_access;
    }

    /// Parse a generic @method(...) call into a InternalCall expression.
    /// Delegates to internal_call_parser.zig
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

        // We're already at the dot, so advance past it
        self.advance();

        // Expect identifier after dot
        if (self.peek().type != .IDENTIFIER) {
            return error.ExpectedIdentifier;
        }

        const member = self.peek();
        self.advance();

        // Look backwards through tokens to find the enum declaration
        var found_valid_variant = false;
        var i: usize = 0; // Start from the beginning
        while (i < self.tokens.len) : (i += 1) {
            if (self.tokens[i].type == .ENUM_TYPE) {
                i += 2;

                // Verify we're at the opening brace
                if (self.tokens[i].type != .LEFT_BRACE) continue;
                i += 1;

                // Scan through variants
                while (i < self.tokens.len and self.tokens[i].type != .RIGHT_BRACE) : (i += 1) {
                    if (self.tokens[i].type == .IDENTIFIER) {
                        if (std.mem.eql(u8, self.tokens[i].lexeme, member.lexeme)) {
                            found_valid_variant = true;
                            break;
                        }
                    }
                }
                if (found_valid_variant) break;
            }
        }

        if (!found_valid_variant) {
            return error.InvalidEnumVariant;
        }

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
        var entries = std.ArrayList(ast.MapEntry).init(self.allocator);
        errdefer {
            for (entries.items) |*entry| {
                entry.key.deinit(self.allocator);
                self.allocator.destroy(entry.key);
                entry.value.deinit(self.allocator);
                self.allocator.destroy(entry.value);
            }
            entries.deinit();
        }

        // Parse entries until we hit a right brace
        while (self.peek().type != .RIGHT_BRACE and self.peek().type != .EOF) {
            // Allow blank lines between entries
            while (self.peek().type == .NEWLINE) self.advance();
            // Parse key as a primary expression only (no complex expressions like maps)
            const key = blk: {
                const token_type = self.peek().type;
                switch (token_type) {
                    .INT, .FLOAT, .STRING, .BYTE, .LOGIC, .IDENTIFIER => {
                        break :blk try precedence.parsePrecedence(self, Precedence.PRIMARY) orelse return error.ExpectedExpression;
                    },
                    .DOT => {
                        // Handle enum members like .SOME_VALUE
                        break :blk try precedence.parsePrecedence(self, Precedence.PRIMARY) orelse return error.ExpectedExpression;
                    },
                    else => return error.ExpectedMapKey,
                }
            };

            // Expect 'is' as separator between key and value
            if (self.peek().type != .ASSIGN) {
                key.deinit(self.allocator);
                self.allocator.destroy(key);
                return error.UseIsForAssignment;
            }
            self.advance(); // consume 'is'

            // Parse value
            const value = try expression_parser.parseExpression(self) orelse return error.ExpectedExpression;

            // Create and append entry
            try entries.append(.{
                .key = key,
                .value = value,
            });

            // Handle separators: comma and/or newline(s)
            if (self.peek().type == .COMMA) {
                self.advance();
            }
            while (self.peek().type == .NEWLINE) self.advance();
            // Allow trailing comma/newlines
        }

        if (self.peek().type != .RIGHT_BRACE) {
            return error.ExpectedRightBrace;
        }
        self.advance();

        // Build Expr.Map for use as an initializer only
        const expr = try self.allocator.create(ast.Expr);
        expr.* = .{
            .base = .{
                .id = ast.generateNodeId(),
                .span = ast.SourceSpan.fromToken(self.peek()),
            },
            .data = .{
                .Map = try entries.toOwnedSlice(),
            },
        };
        return expr;
    }

    pub fn parseBlock(self: *Parser) ErrorList!?*ast.Expr {
        var statements = std.ArrayList(ast.Stmt).init(self.allocator);
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
                    .value = null, // Last expression value
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

    // Helper function to report circular import with full chain
    pub fn reportCircularImport(self: *Parser, current_module: []const u8) ErrorList!ast.ModuleInfo {
        var error_msg = std.ArrayList(u8).init(self.allocator);
        defer error_msg.deinit();

        try error_msg.appendSlice("Circular import detected:\n");

        // Show the import chain
        for (self.import_stack.items) |entry| {
            try error_msg.appendSlice("  ");
            try error_msg.appendSlice(entry.module_path);
            try error_msg.appendSlice(" imports\n");
        }

        // Show the circular dependency
        try error_msg.appendSlice("  ");
        try error_msg.appendSlice(current_module);
        try error_msg.appendSlice(" (circular dependency)\n");

        // Report the error with correct format
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

        // Check if the module name already has an extension
        const has_doxa_ext = std.mem.endsWith(u8, clean_name, ".doxa");
        const has_extension = has_doxa_ext;

        // Get the directory of the current file
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

                // Create a copy of the path for the resolved_path
                const path_copy = try alloc.dupe(u8, file_path);

                return ModuleData{
                    .source = buffer,
                    .resolved_path = path_copy,
                };
            }
        }.read;

        // Try several file locations in order
        var err: anyerror = error.FileNotFound;

        // Prefer resolving relative to the current file first
        // 0a. Same directory as current file with potential existing extension
        if (has_extension) {
            var same_dir_exact_path_pre = std.ArrayList(u8).init(self.allocator);
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
        var same_dir_path_pre = std.ArrayList(u8).init(self.allocator);
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
            var direct_path = std.ArrayList(u8).init(self.allocator);
            defer direct_path.deinit();
            try direct_path.appendSlice(clean_name);

            if (readFileContentsWithPath(self.allocator, direct_path.items)) |data| {
                return data;
            } else |e| {
                err = e;
            }
        }

        // 2. Try in CWD with .doxa extension
        var cwd_doxa_path = std.ArrayList(u8).init(self.allocator);
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
            var same_dir_exact_path = std.ArrayList(u8).init(self.allocator);
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
        var same_dir_path = std.ArrayList(u8).init(self.allocator);
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
            var modules_exact_path = std.ArrayList(u8).init(self.allocator);
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
        var modules_path = std.ArrayList(u8).init(self.allocator);
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

        // Parse the element to push
        const element = try expression_parser.parseExpression(self) orelse return error.ExpectedExpression;

        // Create the array push expression
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

    /// Returns the previous token in the token list
    pub fn previous(self: *Parser) token.Token {
        if (self.current == 0) {
            return self.tokens[0];
        }
        return self.tokens[self.current - 1];
    }

    pub fn arrayPop(self: *Parser, array: ?*ast.Expr, _: Precedence) ErrorList!?*ast.Expr {
        if (array == null) return error.ExpectedExpression;

        // Create the array pop expression
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

        // Parse the second array expression
        const array2 = try expression_parser.parseExpression(self) orelse return error.ExpectedExpression;

        // Don't consume the right paren here - let fieldAccess handle it
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

        // Create the array length expression
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
        self.advance(); // consume 'input' token

        // Check if there's a prompt string (with or without parentheses)
        var prompt: token.Token = token.Token{
            .type = .IDENTIFIER,
            .lexeme = "prompt",
            .literal = .{ .string = "Enter a value: " },
            .line = 0,
            .column = 0,
            .file = "",
        };
        // Handle both input("prompt") and input "prompt" syntax
        if (self.peek().type == .LEFT_PAREN) {
            self.advance(); // consume '('

            if (self.peek().type == .RIGHT_PAREN) {
                // Empty parentheses => no prompt
                self.advance(); // consume ')'
            } else if (self.peek().type == .STRING) {
                prompt = self.peek();
                self.advance(); // consume the string

                // Expect closing parenthesis
                if (self.peek().type != .RIGHT_PAREN) {
                    return error.ExpectedRightParen;
                }
                self.advance(); // consume ')'
            } else {
                return error.ExpectedString;
            }
        } else if (self.peek().type == .STRING) {
            prompt = self.peek();
            self.advance(); // consume the string
        } else {
            // Create an empty prompt if none provided
            prompt = token.Token{
                .file = self.current_file,
                .type = .STRING,
                .lexeme = "",
                .literal = .{ .string = "" },
                .line = self.peek().line,
                .column = self.peek().column,
            };
        }

        // Create the input expression
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
        // Skip self-import check and debug code...

        // 1. Load module if not already loaded (caching at file level)
        const module_info = try module_resolver.resolveModule(self, module_path);

        // 2. Register module in module map using its unique path as identity
        // This creates a module instance accessible by its namespace
        try self.module_namespaces.put(namespace, module_info);

        // 3. Process the module's imports AFTER registering it
        // Each module maintains its own view of its imports
        if (module_info.imports.len > 0) {
            for (module_info.imports) |import| {
                if (import.namespace_alias) |alias| {
                    // Don't create circular imports
                    if (std.mem.eql(u8, import.module_path, self.current_file)) continue;

                    // Create a module-specific import reference
                    // Instead of duplicating the import in the global namespace
                    const qualified_alias = try std.fmt.allocPrint(self.allocator, "{s}.{s}", .{ namespace, alias });
                    defer self.allocator.free(qualified_alias);

                    // Load the imported module only if not already loaded
                    if (!self.module_namespaces.contains(alias)) {
                        // Check if the module is already in the cache
                        if (self.module_cache.contains(import.module_path)) {
                            // Get it from cache and add to namespaces
                            const cached_module = self.module_cache.get(import.module_path).?;
                            try self.module_namespaces.put(alias, cached_module);
                        } else {
                            // Ensure resolution is relative to the importing module's directory
                            const previous_current_file = self.current_file;
                            self.current_file = module_info.file_path;
                            defer self.current_file = previous_current_file;
                            // Only try to load if not in cache either
                            try self.loadAndRegisterModule(import.module_path, alias, import.specific_symbol);
                        }
                    }

                    // Record that this module has access to this import under this alias
                    // This information will be used during symbol resolution
                    try module_resolver.recordModuleImport(self, module_path, alias, import.module_path);
                }
            }
        }

        // 4. Register this module's public symbols in a global symbol table
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
                        // No specific symbol, register all public symbols
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
        // Helper to derive a default namespace alias from the module path (e.g. "test/misc/import.doxa" -> "import")
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

        // First check if module is already cached
        if (self.module_cache.get(module_path)) |module_info| {
            // Ensure the module is registered in namespaces so downstream passes can discover its AST
            const alias = deriveAlias.fromPath(self.allocator, module_path);
            if (!self.module_namespaces.contains(alias)) {
                try self.module_namespaces.put(alias, module_info);
            }

            // Module already loaded, just register the symbol
            if (module_info.ast) |module_ast| {
                try self.registerSpecificSymbol(module_ast, module_path, symbol_name);
            }
            return;
        }

        // Not cached, resolve the module
        const module_info = try module_resolver.resolveModule(self, module_path);

        // Ensure the module is registered in namespaces so downstream passes can discover its AST
        const alias = deriveAlias.fromPath(self.allocator, module_path);
        if (!self.module_namespaces.contains(alias)) {
            try self.module_namespaces.put(alias, module_info);
        }

        // Register the specific symbol
        if (module_info.ast) |module_ast| {
            try self.registerSpecificSymbol(module_ast, module_path, symbol_name);
        }
    }

    // Function to scan a module's AST and register a specific symbol
    fn registerSpecificSymbol(self: *Parser, module_ast: *ast.Expr, module_path: []const u8, symbol_name: []const u8) !void {
        // Initialize symbol table for this module if needed
        if (self.imported_symbols == null) {
            self.imported_symbols = std.StringHashMap(import_parser.ImportedSymbol).init(self.allocator);
        }

        switch (module_ast.data) {
            .Block => {
                const statements = module_ast.data.Block.statements;
                for (statements) |stmt| {
                    switch (stmt.data) {
                        // Handle function declarations
                        .FunctionDecl => |func| {
                            const is_public = func.is_public;
                            if (is_public and std.mem.eql(u8, func.name.lexeme, symbol_name)) {
                                // Register the symbol directly (not with namespace prefix)
                                try self.imported_symbols.?.put(symbol_name, .{
                                    .kind = .Function,
                                    .name = func.name.lexeme,
                                    .original_module = module_path,
                                    .namespace_alias = null,
                                    .param_count = @intCast(func.params.len),
                                    .return_type_info = func.return_type_info,
                                });
                                return; // Found the symbol, we're done
                            }
                        },
                        // Handle variable declarations
                        .VarDecl => |var_decl| {
                            const is_public = var_decl.is_public;
                            if (is_public and std.mem.eql(u8, var_decl.name.lexeme, symbol_name)) {
                                // Register the symbol directly (not with namespace prefix)
                                try self.imported_symbols.?.put(symbol_name, .{
                                    .kind = .Variable,
                                    .name = var_decl.name.lexeme,
                                    .original_module = module_path,
                                });
                                return; // Found the symbol, we're done
                            }
                        },
                        // Handle enum declarations
                        .EnumDecl => |enum_decl| {
                            const is_public = enum_decl.is_public;
                            if (is_public and std.mem.eql(u8, enum_decl.name.lexeme, symbol_name)) {
                                // Register the symbol directly (not with namespace prefix)
                                try self.imported_symbols.?.put(symbol_name, .{
                                    .kind = .Enum,
                                    .name = enum_decl.name.lexeme,
                                    .original_module = module_path,
                                });
                                return; // Found the symbol, we're done
                            }
                        },
                        // Handle struct declarations
                        .Expression => |maybe_expr| {
                            if (maybe_expr) |expr| {
                                if (expr.data == .StructDecl) {
                                    const struct_decl = expr.data.StructDecl;
                                    const is_public = struct_decl.is_public;
                                    if (is_public and std.mem.eql(u8, struct_decl.name.lexeme, symbol_name)) {
                                        // Register the symbol directly (not with namespace prefix)
                                        try self.imported_symbols.?.put(symbol_name, .{
                                            .kind = .Struct,
                                            .name = struct_decl.name.lexeme,
                                            .original_module = module_path,
                                        });
                                        return; // Found the symbol, we're done
                                    }
                                }
                            }
                        },
                        else => {}, // Skip other types of statements
                    }
                }
            },
            else => {}, // Skip other AST types
        }
    }
};
