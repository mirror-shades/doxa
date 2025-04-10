const std = @import("std");
const ast = @import("../ast.zig");
const ModuleInfo = @import("../ast.zig").ModuleInfo;
const token = @import("../token.zig");
const declaration_parser = @import("declaration_parser.zig");
const expression_parser = @import("expression_parser.zig");
const statement_parser = @import("statement_parser.zig");
const Reporting = @import("../reporting.zig");
const ErrorList = Reporting.ErrorList;
const Reporter = Reporting.Reporter;
const Precedence = @import("./precedence.zig").Precedence;
const Lexer = @import("../lexer.zig").Lexer;
const import_parser = @import("import_parser.zig");

const TokenStyle = enum {
    Keyword,
    Symbol,
    Undefined,
};

pub const Parser = struct {
    tokens: []const token.Token,
    current: usize,
    allocator: std.mem.Allocator,
    debug_enabled: bool,
    current_file: []const u8,

    // Add these fields to track entry points
    has_entry_point: bool = false,
    entry_point_location: ?token.Token = null,
    entry_point_name: ?[]const u8 = null,

    current_module: ?ModuleInfo = null,
    module_cache: std.StringHashMap(ModuleInfo),
    module_namespaces: std.StringHashMap(ModuleInfo),

    // Track imported symbols
    imported_symbols: ?std.StringHashMap(import_parser.ImportedSymbol) = null,

    // Add lexer field
    lexer: Lexer,

    declared_types: std.StringHashMap(void),

    pub fn init(allocator: std.mem.Allocator, tokens: []const token.Token, current_file: []const u8, debug_enabled: bool) Parser {
        const parser = Parser{
            .allocator = allocator,
            .tokens = tokens,
            .current = 0,
            .debug_enabled = debug_enabled,
            .current_file = current_file,
            .module_cache = std.StringHashMap(ModuleInfo).init(allocator),
            .module_namespaces = std.StringHashMap(ModuleInfo).init(allocator),
            .imported_symbols = std.StringHashMap(import_parser.ImportedSymbol).init(allocator),
            .lexer = Lexer.init(allocator, "", current_file),
            .declared_types = std.StringHashMap(void).init(allocator),
        };

        return parser;
    }

    pub fn deinit(_: *Parser) void {}

    pub fn peek(self: *Parser) token.Token {
        return self.tokens[self.current];
    }

    pub fn peekAhead(self: *Parser, offset: usize) token.Token {
        const pos = self.current + offset;
        if (pos >= self.tokens.len) {
            return self.tokens[self.tokens.len - 1];
        }
        return self.tokens[pos];
    }

    pub fn advance(self: *Parser) void {
        // Only advance if we're not at the end
        if (self.current < self.tokens.len - 1) {
            self.current += 1;
        }
    }

    pub fn block(self: *Parser, _: ?*ast.Expr, _: Precedence) ErrorList!?*ast.Expr {
        if (self.debug_enabled) {
            std.debug.print("\nParsing block expression...\n", .{});
        }

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

        const last_expr: ?*ast.Expr = null;

        while (self.peek().type != .RIGHT_BRACE and self.peek().type != .EOF) {
            // Parse any statement type
            const stmt = try statement_parser.parseStatement(self);
            try statements.append(stmt);

            // Break after return statement
            if (stmt == .Return) break;

            // Don't break on semicolon before right brace
            if (self.peek().type == .RIGHT_BRACE) {
                break;
            }
        }

        if (self.peek().type != .RIGHT_BRACE) {
            return error.ExpectedRightBrace;
        }
        self.advance();

        const block_expr = try self.allocator.create(ast.Expr);
        block_expr.* = .{ .Block = .{
            .statements = try statements.toOwnedSlice(),
            .value = last_expr,
        } };

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

        // --- Pass 1: Imports (Process side effects, don't add to statements list yet) ---
        // Keep track of the original position after imports are processed
        var start_index_after_imports = self.current;
        while (self.tokens[start_index_after_imports].type == .IMPORT) {
            // Temporarily move parser to process the import
            const temp_current = self.current;
            self.current = start_index_after_imports;

            _ = try import_parser.parseImportStmt(self); // Call for side-effects (loading/registration)

            // Advance start_index_after_imports past the import statement
            // parseImportStmt should have advanced self.current appropriately
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

            // IMPORTANT: Check for MAIN first as it might appear before PUBLIC
            if (self.peek().type == .MAIN) {
                is_entry = true;
                const entry_token = self.peek(); // Store token for later use if needed
                self.advance(); // consume ->
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
                    decl.VarDecl.is_public = is_public; // Apply modifier
                    // Cannot be entry point
                    if (is_entry) {
                        return error.InvalidEntryPoint;
                    }
                    try statements.append(decl);
                },
                .FUNCTION => {
                    var func = try declaration_parser.parseFunctionDecl(self);
                    func.Function.is_public = is_public; // Apply modifiers
                    func.Function.is_entry = is_entry;
                    if (is_entry) {
                        // Store entry point name if this function is the entry point
                        // Check if location was already set by '->' token earlier
                        if (self.entry_point_location != null) {
                            self.entry_point_name = func.Function.name.lexeme;
                        } else {
                            // This case means func keyword without preceding '->', shouldn't happen if logic is correct
                            {
                                return error.MultipleEntryPoints;
                            }
                        }
                    }
                    try statements.append(func);
                },
                .STRUCT_TYPE => {
                    const expr = try declaration_parser.parseStructDecl(self, null, .NONE);
                    // Apply is_public to struct decl in AST
                    if (expr) |non_null_expr| {
                        if (non_null_expr.* == .StructDecl) {
                            non_null_expr.StructDecl.is_public = is_public;
                        }
                    }
                    // Cannot be entry point
                    if (is_entry) {
                        return error.InvalidEntryPoint;
                    }
                    // Append only if expr is not null
                    if (expr) |e| {
                        try statements.append(.{ .Expression = e });
                    }
                },
                .ENUM_TYPE => {
                    var enum_decl = try declaration_parser.parseEnumDecl(self);
                    enum_decl.EnumDecl.is_public = is_public; // Apply modifier
                    // Cannot be entry point
                    if (is_entry) {
                        return error.InvalidEntryPoint;
                    }
                    try statements.append(enum_decl);
                },
                // Handle other statement types recognised by statement_parser
                .IF, .WHILE, .RETURN, .LEFT_BRACE, .TRY => {
                    // Modifiers likely invalid here
                    if (is_entry) {
                        return error.MisplacedEntryPoint;
                    }
                    if (is_public) {
                        return error.MisplacedPublicModifier;
                    }
                    const parsed_stmt = try statement_parser.parseStatement(self);
                    // Only append if it's not a null expression (e.g. from empty block)
                    if (!(parsed_stmt == .Expression and parsed_stmt.Expression == null)) {
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
                    if (!(expr_stmt == .Expression and expr_stmt.Expression == null)) {
                        try statements.append(expr_stmt);
                    }
                },
            }

            // --- Progress Check ---
            // Ensure the parser advanced. If not, it means a sub-parser failed to consume tokens.
            if (self.current == loop_start_pos and self.peek().type != .EOF) {
                // Log error, force advance, or return error? Returning error is safest.
                std.debug.print("Parser did not advance at token index {d}, type: {s}\n", .{ self.current, @tagName(self.peek().type) });
                return error.ParserDidNotAdvance;
            }
        }

        // Final check: If entry point marker '->' was found but no function followed
        if (self.has_entry_point and self.entry_point_name == null) {
            // Report error using self.entry_point_location
            var reporting = Reporter.init();
            reporting.reportCompileError(.{
                .file = self.current_file,
                .line = self.entry_point_location.?.line,
                .column = self.entry_point_location.?.column,
            }, "Entry point marker '->' not followed by a function declaration", .{});
            return error.MissingEntryPointFunction;
        }

        if (self.debug_enabled) {
            std.debug.print("\n=== Parse complete, statement count: {} ===\n", .{statements.items.len});
            for (statements.items, 0..) |st, i| {
                std.debug.print("Statement {}: {s}\n", .{ i, @tagName(st) });
            }
        }

        return statements.toOwnedSlice();
    }

    pub fn parseParameters(self: *Parser, params: *std.ArrayList(ast.FunctionParam)) ErrorList!void {
        if (self.debug_enabled) {
            std.debug.print("Parsing function parameters...\n", .{});
        }

        while (true) {
            if (self.debug_enabled) {
                std.debug.print("Current token: {s} ({s})\n", .{ @tagName(self.peek().type), self.peek().lexeme });
            }

            if (self.peek().type != .IDENTIFIER) {
                var reporter = Reporter.init();
                reporter.reportCompileError(.{
                    .file = self.current_file,
                    .line = self.peek().line,
                    .column = self.peek().column,
                }, "Expected identifier, but found '{s}'", .{@tagName(self.peek().type)});
                return error.ExpectedIdentifier;
            }

            const param_name = self.peek();
            self.advance();

            // Parse type annotation if present
            var type_expr: ?*ast.TypeExpr = null;
            if (self.peek().type == .TYPE_SYMBOL) {
                self.advance(); // consume ::
                if (self.debug_enabled) {
                    std.debug.print("Parsing type expression, current token: {s} ({s})\n", .{ @tagName(self.peek().type), self.peek().lexeme });
                }
                type_expr = try expression_parser.parseTypeExpr(self);
                if (self.debug_enabled) {
                    std.debug.print("Finished parsing type expression\n", .{});
                }
            }

            // Handle default value
            var default_value: ?*ast.Expr = null;
            if (self.peek().type == .ASSIGN) {
                self.advance(); // consume =
                const value_expr = try expression_parser.parseExpression(self) orelse return error.ExpectedExpression;
                const default_literal = try self.allocator.create(ast.Expr);
                default_literal.* = .{ .Literal = value_expr.Literal };
                default_value = default_literal;
            }

            try params.append(.{
                .name = param_name,
                .type_expr = type_expr,
                .default_value = default_value,
            });

            if (self.peek().type == .COMMA) {
                if (self.debug_enabled) {
                    std.debug.print("Found comma, continuing to next parameter\n", .{});
                }
                self.advance();
                continue;
            }
            break;
        }

        if (self.debug_enabled) {
            std.debug.print("Parsed {} parameters\n", .{params.items.len});
        }
    }

    pub fn hasReturnWithValue(self: *Parser) !bool {
        // Find the opening brace of the function body
        var pos = self.current;
        while (pos < self.tokens.len and self.tokens[pos].type != .LEFT_BRACE) {
            pos += 1;
        }
        if (pos >= self.tokens.len) return false;

        // Start scanning after the opening brace
        pos += 1;
        var brace_count: usize = 1;
        var found_return_value = false;

        if (self.debug_enabled) {
            std.debug.print("Scanning for return values starting at position {}\n", .{pos});
        }

        while (pos < self.tokens.len) {
            const current_token = self.tokens[pos];

            if (self.debug_enabled) {
                std.debug.print("Scanning token: {s} at position {}\n", .{ @tagName(current_token.type), pos });
            }

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
                        if (next_token.type != .SEMICOLON) {
                            found_return_value = true;
                            break;
                        }
                    }
                },
                else => {},
            }

            pos += 1;
        }

        if (self.debug_enabled) {
            std.debug.print("Scan complete. Found return value: {}\n", .{found_return_value});
        }

        return found_return_value;
    }

    pub fn call(self: *Parser, callee: ?*ast.Expr, _: Precedence) ErrorList!?*ast.Expr {
        if (self.debug_enabled) {
            std.debug.print("\nParsing function call...\n", .{});
        }

        // We're already at the opening parenthesis, advance past it
        self.advance(); // consume (

        var arguments = std.ArrayList(*ast.Expr).init(self.allocator);
        errdefer {
            for (arguments.items) |arg| {
                arg.deinit(self.allocator);
                self.allocator.destroy(arg);
            }
            arguments.deinit();
        }

        // Handle empty argument list
        if (self.peek().type == .RIGHT_PAREN) {
            self.advance(); // consume )
            const call_expr = try self.allocator.create(ast.Expr);
            call_expr.* = .{ .Call = .{
                .callee = callee.?,
                .arguments = try arguments.toOwnedSlice(),
            } };
            return call_expr;
        }

        // Parse arguments
        while (true) {
            var arg: *ast.Expr = undefined;
            if (self.peek().type == .TILDE) {
                // Handle default argument placeholder
                self.advance(); // consume ~
                const placeholder = try self.allocator.create(ast.Expr);
                placeholder.* = .DefaultArgPlaceholder; // Added semicolon here
                arg = placeholder;
            } else {
                arg = try expression_parser.parseExpression(self) orelse return error.ExpectedExpression;
            }
            try arguments.append(arg);

            if (self.peek().type == .RIGHT_PAREN) break;
            if (self.peek().type != .COMMA) return error.ExpectedComma;
            self.advance(); // consume comma
        }

        if (self.debug_enabled) {
            std.debug.print("Looking for closing paren, current token: {s}\n", .{@tagName(self.peek().type)});
        }

        // Expect closing parenthesis
        if (self.peek().type != .RIGHT_PAREN) {
            if (self.debug_enabled) {
                std.debug.print("Expected ), got {s}\n", .{@tagName(self.peek().type)});
            }
            return error.ExpectedRightParen;
        }
        self.advance(); // consume )

        // Create call expression
        const call_expr = try self.allocator.create(ast.Expr);
        call_expr.* = .{ .Call = .{
            .callee = callee.?,
            .arguments = try arguments.toOwnedSlice(),
        } };

        return call_expr;
    }

    pub fn parseStructInit(self: *Parser) ErrorList!?*ast.Expr {
        if (self.debug_enabled) {
            std.debug.print("\nTrying to parse struct init at position {}, token: {s}\n", .{
                self.current,
                @tagName(self.peek().type),
            });
        }

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
                    // Parse field name
                    if (self.peek().type != .IDENTIFIER) {
                        return error.ExpectedIdentifier;
                    }
                    const field_name = self.peek();
                    self.advance();

                    // Expect =
                    if (self.peek().type != .ASSIGN) {
                        return error.ExpectedEquals;
                    }
                    self.advance();

                    // Parse field value
                    const value = try expression_parser.parseExpression(self) orelse return error.ExpectedExpression;

                    // Create field
                    const field = try self.allocator.create(ast.StructInstanceField);
                    field.* = .{
                        .name = field_name,
                        .value = value,
                    };
                    try fields.append(field);

                    // Handle comma
                    if (self.peek().type == .COMMA) {
                        self.advance();
                        // Allow trailing comma
                        if (self.peek().type == .RIGHT_BRACE) {
                            break;
                        }
                    } else if (self.peek().type != .RIGHT_BRACE) {
                        return error.ExpectedCommaOrBrace;
                    }
                }

                self.advance(); // consume }

                // Create struct literal
                const struct_init = try self.allocator.create(ast.Expr);
                struct_init.* = .{ .StructLiteral = .{
                    .name = type_name,
                    .fields = try fields.toOwnedSlice(),
                } };
                return struct_init;
            }
        }

        if (self.peek().type != .LEFT_BRACE) {
            if (self.debug_enabled) {
                std.debug.print("No left brace found, resetting to position {}\n", .{start_pos});
            }
            // Reset position if this isn't a struct initialization
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
            if (self.debug_enabled) {
                std.debug.print("Parsing field at position {}, token: {s}\n", .{
                    self.current,
                    @tagName(self.peek().type),
                });
            }

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
                if (self.debug_enabled) {
                    std.debug.print("Attempting nested struct init for identifier: {s}\n", .{
                        self.peek().lexeme,
                    });
                }
                if (try parseStructInit(self)) |struct_init| {
                    value = struct_init;
                } else {
                    if (self.debug_enabled) {
                        std.debug.print("Not a struct init, falling back to expression\n", .{});
                    }
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

            // Handle comma
            if (self.peek().type == .COMMA) {
                self.advance();
                // Allow trailing comma by checking for closing brace
                if (self.peek().type == .RIGHT_BRACE) {
                    break;
                }
            } else if (self.peek().type != .RIGHT_BRACE) {
                if (self.debug_enabled) {
                    std.debug.print("Expected comma or brace, got: {s}\n", .{
                        @tagName(self.peek().type),
                    });
                }
                return error.ExpectedCommaOrBrace;
            }
        }

        self.advance(); // consume }

        if (self.debug_enabled) {
            std.debug.print("Successfully parsed struct init for {s}\n", .{struct_name.lexeme});
        }

        const struct_init = try self.allocator.create(ast.Expr);
        struct_init.* = .{ .StructLiteral = .{
            .name = struct_name,
            .fields = try fields.toOwnedSlice(),
        } };
        return struct_init;
    }

    pub fn index(self: *Parser, array_expr: ?*ast.Expr, _: Precedence) ErrorList!?*ast.Expr {
        if (self.debug_enabled) {
            std.debug.print("Parsing array/tuple index\n", .{});
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
            expr.* = .{ .IndexAssign = .{
                .array = array_expr.?,
                .index = index_expr,
                .value = value,
            } };
            return expr;
        }

        const expr = try self.allocator.create(ast.Expr);
        expr.* = .{ .Index = .{
            .array = array_expr.?,
            .index = index_expr,
        } };

        // Check for another index operation (for nested access)
        if (self.peek().type == .LEFT_BRACKET) {
            self.advance(); // consume [
            return self.index(expr, .NONE);
        }

        return expr;
    }

    pub fn assignment(self: *Parser, left: ?*ast.Expr, _: Precedence) ErrorList!?*ast.Expr {
        if (self.debug_enabled) {
            std.debug.print("\nParsing assignment...\n", .{});
            std.debug.print("Current token: {s} at position {}\n", .{
                @tagName(self.peek().type),
                self.current,
            });
        }

        if (left == null) return error.ExpectedExpression;

        // Add check for equals sign
        if (self.previous().type != .ASSIGN) {
            return error.UseIsForAssignment;
        }

        // Handle other expressions
        const value = try expression_parser.parseExpression(self) orelse return error.ExpectedExpression;

        // Check if left side is a valid assignment target
        switch (left.?.*) {
            .Variable => |name| {
                const assign = try self.allocator.create(ast.Expr);
                assign.* = .{ .Assignment = .{
                    .name = name,
                    .value = value,
                } };
                return assign;
            },
            .FieldAccess => |field_access| {
                const assign = try self.allocator.create(ast.Expr);
                assign.* = .{ .FieldAssignment = .{
                    .object = field_access.object,
                    .field = field_access.field,
                    .value = value,
                } };
                return assign;
            },
            .Index => |index_expr| {
                const assign = try self.allocator.create(ast.Expr);
                assign.* = .{ .IndexAssign = .{
                    .array = index_expr.array,
                    .index = index_expr.index,
                    .value = value,
                } };
                return assign;
            },
            else => return error.InvalidAssignmentTarget,
        }
    }

    pub fn fieldAccess(self: *Parser, left: ?*ast.Expr, _: Precedence) ErrorList!?*ast.Expr {
        if (self.debug_enabled) {
            std.debug.print("\nStarting field access parse\n", .{});
            std.debug.print("Current position: {}\n", .{self.current});
            std.debug.print("Current token: {s} ({s})\n", .{
                @tagName(self.peek().type),
                self.peek().lexeme,
            });
        }

        // Store current position and token
        const current_token = self.peek();

        // Handle length as a property access
        if (std.mem.eql(u8, current_token.lexeme, "length") or
            std.mem.eql(u8, current_token.lexeme, "push") or
            std.mem.eql(u8, current_token.lexeme, "pop") or
            std.mem.eql(u8, current_token.lexeme, "isEmpty") or
            std.mem.eql(u8, current_token.lexeme, "concat"))
        {
            self.advance(); // consume length

            const field_access = try self.allocator.create(ast.Expr);
            field_access.* = .{
                .FieldAccess = .{
                    .object = left.?,
                    .field = current_token,
                },
            };
            return field_access;
        }

        // Handle bytes as a property access
        if (std.mem.eql(u8, current_token.lexeme, "bytes")) {
            self.advance(); // consume bytes

            // Create a field access expression
            const field_access = try self.allocator.create(ast.Expr);
            field_access.* = .{
                .FieldAccess = .{
                    .object = left.?,
                    .field = current_token,
                },
            };

            // Check if there's an array index following
            if (self.peek().type == .LEFT_BRACKET) {
                self.advance(); // consume [
                const idx_expr = try expression_parser.parseExpression(self) orelse return error.ExpectedExpression;

                if (self.peek().type != .RIGHT_BRACKET) {
                    return error.ExpectedRightBracket;
                }
                self.advance(); // consume ]

                // Create an Index expression wrapping the field access
                const index_expr = try self.allocator.create(ast.Expr);
                index_expr.* = .{
                    .Index = .{
                        .array = field_access,
                        .index = idx_expr,
                    },
                };
                return index_expr;
            }

            return field_access;
        }

        // Handle regular field access
        if (current_token.type == .IDENTIFIER) {
            self.advance(); // consume identifier

            const field_access = try self.allocator.create(ast.Expr);
            field_access.* = .{
                .FieldAccess = .{
                    .object = left.?,
                    .field = current_token,
                },
            };

            // Check if there's an index operation after the field access
            if (self.peek().type == .LEFT_BRACKET) {
                self.advance(); // consume [
                return try self.index(field_access, .NONE);
            }

            return field_access;
        } else {
            if (self.debug_enabled) {
                std.debug.print("Invalid token: {s} ({s})\n", .{
                    @tagName(current_token.type),
                    current_token.lexeme,
                });
            }
            return error.ExpectedIdentifier;
        }
    }

    pub fn print(self: *Parser, left: ?*ast.Expr, _: Precedence) ErrorList!?*ast.Expr {
        if (left == null) return error.ExpectedExpression;

        var name_token: ?token.Token = null;
        if (left.?.* == .Variable) {
            name_token = left.?.Variable;
        }

        const print_expr = try self.allocator.create(ast.Expr);
        print_expr.* = .{
            .Print = .{
                .expr = left.?,
                .location = .{
                    .file = self.current_file,
                    .line = self.peek().line,
                    .column = self.peek().column - 1,
                },
                .variable_name = if (name_token) |token_name| token_name.lexeme else null,
            },
        };

        return print_expr;
    }

    pub fn enumMember(self: *Parser, _: ?*ast.Expr, _: Precedence) ErrorList!?*ast.Expr {
        if (self.debug_enabled) {
            std.debug.print("\nParsing enum member access...\n", .{});
        }

        // We're already at the dot, so advance past it
        self.advance();

        // Expect identifier after dot
        if (self.peek().type != .IDENTIFIER) {
            return error.ExpectedIdentifier;
        }

        const member = self.peek();
        self.advance();

        // Debug output for token scanning
        if (self.debug_enabled) {
            std.debug.print("\nLooking for enum variant '{s}'...\n", .{member.lexeme});
        }

        // Look backwards through tokens to find the enum declaration
        var found_valid_variant = false;
        var i: usize = 0; // Start from the beginning
        while (i < self.tokens.len) : (i += 1) {
            if (self.debug_enabled) {
                std.debug.print("Checking token at {}: {s} ({s})\n", .{ i, @tagName(self.tokens[i].type), self.tokens[i].lexeme });
            }

            if (self.tokens[i].type == .ENUM_TYPE) {
                if (self.debug_enabled) {
                    std.debug.print("Found enum declaration at {}\n", .{i});
                }

                // Skip enum keyword and name
                i += 2;

                // Verify we're at the opening brace
                if (self.tokens[i].type != .LEFT_BRACE) continue;
                i += 1;

                // Scan through variants
                while (i < self.tokens.len and self.tokens[i].type != .RIGHT_BRACE) : (i += 1) {
                    if (self.tokens[i].type == .IDENTIFIER) {
                        if (self.debug_enabled) {
                            std.debug.print("Checking variant: {s}\n", .{self.tokens[i].lexeme});
                        }
                        if (std.mem.eql(u8, self.tokens[i].lexeme, member.lexeme)) {
                            found_valid_variant = true;
                            if (self.debug_enabled) {
                                std.debug.print("Found matching variant!\n", .{});
                            }
                            break;
                        }
                    }
                }
                if (found_valid_variant) break;
            }
        }

        if (!found_valid_variant) {
            if (self.debug_enabled) {
                std.debug.print("Invalid enum variant: {s}\n", .{member.lexeme});
            }
            return error.InvalidEnumVariant;
        }

        const enum_member = try self.allocator.create(ast.Expr);
        enum_member.* = .{ .EnumMember = member };
        return enum_member;
    }

    pub fn parseTuple(self: *Parser, _: ?*ast.Expr, _: Precedence) ErrorList!?*ast.Expr {
        if (self.debug_enabled) {
            std.debug.print("\nParsing tuple...\n", .{});
        }

        self.advance(); // consume '('

        var elements = std.ArrayList(*ast.Expr).init(self.allocator);
        errdefer {
            if (self.debug_enabled) {
                std.debug.print("Starting tuple cleanup with {} elements\n", .{elements.items.len});
            }
            // Safe cleanup of any successfully parsed elements
            if (elements.items.len > 0) {
                for (elements.items) |element| {
                    if (self.debug_enabled) {
                        std.debug.print("About to clean up element of type {s}\n", .{
                            @tagName(element.*),
                        });
                    }
                    element.deinit(self.allocator);
                    self.allocator.destroy(element);
                    if (self.debug_enabled) {
                        std.debug.print("Element cleanup complete\n", .{});
                    }
                }
            }
            elements.deinit();
            if (self.debug_enabled) {
                std.debug.print("Tuple cleanup complete\n", .{});
            }
        }

        // Try to parse first element with error handling
        if (self.debug_enabled) {
            std.debug.print("About to parse first element\n", .{});
        }

        const first = expression_parser.parseExpression(self) catch |err| {
            if (self.debug_enabled) {
                std.debug.print("Error parsing first element: {}\n", .{err});
            }
            return err;
        } orelse {
            if (self.debug_enabled) {
                std.debug.print("No expression returned\n", .{});
            }
            return error.ExpectedExpression;
        };

        if (self.debug_enabled) {
            std.debug.print("Successfully parsed first element\n", .{});
        }

        try elements.append(first);

        // If there's a comma, this is a tuple. Otherwise, it's just a grouped expression
        if (self.peek().type == .COMMA) {
            // Parse remaining elements
            while (true) {
                if (self.peek().type == .COMMA) {
                    self.advance(); // consume comma

                    // Allow trailing comma
                    if (self.peek().type == .RIGHT_PAREN) break;

                    const element = try expression_parser.parseExpression(self) orelse return error.ExpectedExpression;
                    try elements.append(element);
                } else if (self.peek().type == .RIGHT_PAREN) {
                    break;
                } else {
                    return error.ExpectedCommaOrParen;
                }
            }

            self.advance(); // consume ')'

            const tuple_expr = try self.allocator.create(ast.Expr);
            tuple_expr.* = .{ .Tuple = try elements.toOwnedSlice() };
            return tuple_expr;
        } else {
            // Not a tuple, just a grouped expression
            elements.deinit();

            if (self.peek().type != .RIGHT_PAREN) {
                first.deinit(self.allocator);
                self.allocator.destroy(first);
                return error.ExpectedRightParen;
            }
            self.advance(); // consume ')'

            const grouping_expr = try self.allocator.create(ast.Expr);
            grouping_expr.* = .{ .Grouping = first };
            return grouping_expr;
        }
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
            // Parse key
            const key = try expression_parser.parseExpression(self) orelse return error.ExpectedExpression;

            // Expect :
            if (self.peek().type != .WHERE) {
                key.deinit(self.allocator);
                self.allocator.destroy(key);
                return error.ExpectedColon;
            }
            self.advance(); // consume :

            // Parse value
            const value = try expression_parser.parseExpression(self) orelse return error.ExpectedExpression;

            // Create and append entry
            try entries.append(.{
                .key = key,
                .value = value,
            });

            // Handle comma
            if (self.peek().type == .COMMA) {
                self.advance();
                // Allow trailing comma
                if (self.peek().type == .RIGHT_BRACE) break;
            } else if (self.peek().type != .RIGHT_BRACE) {
                return error.ExpectedCommaOrBrace;
            }
        }

        if (self.peek().type != .RIGHT_BRACE) {
            return error.ExpectedRightBrace;
        }
        self.advance();

        const map_expr = try self.allocator.create(ast.Expr);
        map_expr.* = .{ .Map = try entries.toOwnedSlice() };
        return map_expr;
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

            // Don't break on semicolon before right brace
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
            .Block = .{
                .statements = try statements.toOwnedSlice(),
                .value = null, // Last expression value
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

    pub fn resolveModule(self: *Parser, module_name: []const u8) ErrorList!ast.ModuleInfo {
        // Check module cache first
        if (self.module_cache.get(module_name)) |info| {
            return info;
        }

        // Load and parse module file
        const module_source = try self.loadModuleSource(module_name);
        var module_lexer = Lexer.init(self.allocator, module_source, module_name);
        defer module_lexer.deinit();

        try module_lexer.initKeywords();
        const tokens = try module_lexer.lexTokens();

        var new_parser = Parser.init(self.allocator, tokens.items, module_name, self.debug_enabled);

        // Parse the module file
        const module_statements = try new_parser.execute();

        // Create a block expression to hold the module statements
        const module_block = try self.allocator.create(ast.Expr);
        module_block.* = .{ .Block = .{
            .statements = module_statements,
            .value = null,
        } };

        // Extract module info and cache it
        const info = try self.extractModuleInfo(module_block, module_name, null);
        try self.module_cache.put(module_name, info);

        return info;
    }

    pub fn loadModuleSource(self: *Parser, module_name: []const u8) ErrorList![]const u8 {
        if (self.debug_enabled) {
            std.debug.print("Loading module: {s}\n", .{module_name});
        }

        // Remove ./ prefix if present, since we'll be constructing paths properly
        var clean_name = module_name;
        if (std.mem.startsWith(u8, clean_name, "./")) {
            clean_name = clean_name[2..];
            if (self.debug_enabled) {
                std.debug.print("Removed ./ prefix, clean name: {s}\n", .{clean_name});
            }
        }

        // Check if the module name already has an extension
        const has_doxa_ext = std.mem.endsWith(u8, clean_name, ".doxa");
        const has_extension = has_doxa_ext;

        if (self.debug_enabled) {
            std.debug.print("Module already has extension: {}\n", .{has_extension});
        }

        // Get the directory of the current file
        const current_dir = std.fs.path.dirname(self.current_file) orelse ".";

        if (self.debug_enabled) {
            std.debug.print("Current directory: {s}\n", .{current_dir});
        }

        // Capture debug flag for the inner function
        const debug_enabled = self.debug_enabled;

        // We'll use this function to read a file
        const readFileContents = struct {
            fn read(alloc: std.mem.Allocator, file_path: []const u8, debug: bool) ![]u8 {
                if (debug) {
                    std.debug.print("Attempting to read: {s}\n", .{file_path});
                }

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

                return buffer;
            }
        }.read;

        // Try several file locations in order
        var err: anyerror = error.FileNotFound;

        // 1. First try directly with the given name (possibly including extension)
        if (has_extension) {
            var direct_path = std.ArrayList(u8).init(self.allocator);
            defer direct_path.deinit();
            try direct_path.appendSlice(clean_name);

            if (readFileContents(self.allocator, direct_path.items, debug_enabled)) |buffer| {
                return buffer;
            } else |e| {
                err = e;
                if (self.debug_enabled) {
                    std.debug.print("Failed to open {s} with existing extension: {}\n", .{ direct_path.items, e });
                }
            }
        }

        // 2. Try in CWD with .doxa extension
        var cwd_doxa_path = std.ArrayList(u8).init(self.allocator);
        defer cwd_doxa_path.deinit();
        try cwd_doxa_path.appendSlice(clean_name);
        if (!has_extension) try cwd_doxa_path.appendSlice(".doxa");

        if (readFileContents(self.allocator, cwd_doxa_path.items, debug_enabled)) |buffer| {
            return buffer;
        } else |e| {
            err = e;
            if (self.debug_enabled) {
                std.debug.print("Failed to open {s} in current working directory: {}\n", .{ cwd_doxa_path.items, e });
            }
        }

        // 3. Try in the same directory as the current file with potential existing extension
        if (has_extension) {
            var same_dir_exact_path = std.ArrayList(u8).init(self.allocator);
            defer same_dir_exact_path.deinit();
            try same_dir_exact_path.appendSlice(current_dir);
            try same_dir_exact_path.appendSlice("/");
            try same_dir_exact_path.appendSlice(clean_name);

            if (readFileContents(self.allocator, same_dir_exact_path.items, debug_enabled)) |buffer| {
                return buffer;
            } else |e| {
                err = e;
                if (self.debug_enabled) {
                    std.debug.print("Failed to open {s}: {}\n", .{ same_dir_exact_path.items, e });
                }
            }
        }

        // 4. Try in the same directory as the current file with .doxa extension
        var same_dir_path = std.ArrayList(u8).init(self.allocator);
        defer same_dir_path.deinit();
        try same_dir_path.appendSlice(current_dir);
        try same_dir_path.appendSlice("/");
        try same_dir_path.appendSlice(clean_name);
        if (!has_extension) try same_dir_path.appendSlice(".doxa");

        if (self.debug_enabled) {
            std.debug.print("Trying: {s}\n", .{same_dir_path.items});
        }

        if (readFileContents(self.allocator, same_dir_path.items, debug_enabled)) |buffer| {
            return buffer;
        } else |e| {
            err = e;
            if (self.debug_enabled) {
                std.debug.print("Failed to open {s}: {}\n", .{ same_dir_path.items, e });
            }
        }

        // 5. Try the modules subdirectory (with potential existing extension)
        if (has_extension) {
            var modules_exact_path = std.ArrayList(u8).init(self.allocator);
            defer modules_exact_path.deinit();
            try modules_exact_path.appendSlice(current_dir);
            try modules_exact_path.appendSlice("/modules/");
            try modules_exact_path.appendSlice(clean_name);

            if (readFileContents(self.allocator, modules_exact_path.items, debug_enabled)) |buffer| {
                return buffer;
            } else |e| {
                err = e;
                if (self.debug_enabled) {
                    std.debug.print("Failed to open {s}: {}\n", .{ modules_exact_path.items, e });
                }
            }
        }

        // 6. Try the modules subdirectory with .doxa extension
        var modules_path = std.ArrayList(u8).init(self.allocator);
        defer modules_path.deinit();
        try modules_path.appendSlice(current_dir);
        try modules_path.appendSlice("/modules/");
        try modules_path.appendSlice(clean_name);
        if (!has_extension) try modules_path.appendSlice(".doxa");

        if (readFileContents(self.allocator, modules_path.items, debug_enabled)) |buffer| {
            return buffer;
        } else |e| {
            err = e;
            if (self.debug_enabled) {
                std.debug.print("Failed to open {s}: {}\n", .{ modules_path.items, e });
            }
        }

        // If we reach here, all attempts failed
        if (self.debug_enabled) {
            std.debug.print("All paths failed for module: {s}\n", .{module_name});
            std.debug.print("Current directory: {s}\n", .{current_dir});
        }

        return error.ModuleNotFound;
    }

    pub fn arrayPush(self: *Parser, array: ?*ast.Expr, _: Precedence) ErrorList!?*ast.Expr {
        if (array == null) return error.ExpectedExpression;

        // Parse the element to push
        const element = try expression_parser.parseExpression(self) orelse return error.ExpectedExpression;

        // Create the array push expression
        const push_expr = try self.allocator.create(ast.Expr);
        push_expr.* = .{ .ArrayPush = .{
            .array = array.?,
            .element = element,
        } };

        return push_expr;
    }

    pub fn arrayIsEmpty(self: *Parser, array: ?*ast.Expr, _: Precedence) ErrorList!?*ast.Expr {
        if (array == null) return error.ExpectedExpression;

        const empty_expr = try self.allocator.create(ast.Expr);
        empty_expr.* = .{ .ArrayIsEmpty = .{ .array = array.? } };
        return empty_expr;
    }

    pub fn extractModuleInfo(self: *Parser, module_ast: *ast.Expr, module_path: []const u8, specific_symbol: ?[]const u8) !ast.ModuleInfo {
        if (self.debug_enabled) {
            std.debug.print("\n=== Starting module info extraction ===\n", .{});
            std.debug.print("Module path: {s}\n", .{module_path});
            std.debug.print("Specific symbol: {?s}\n", .{specific_symbol});
            std.debug.print("Module AST type: {s}\n", .{@tagName(module_ast.*)});
            if (module_ast.* == .Block) {
                std.debug.print("Block has {d} statements\n", .{module_ast.Block.statements.len});
            }
        }

        var imports = std.ArrayList(ast.ImportInfo).init(self.allocator);
        var name: []const u8 = "";

        // Extract module name from path
        var path_iter = std.mem.splitSequence(u8, module_path, "/");
        var last_part: []const u8 = "";
        while (path_iter.next()) |part| {
            last_part = part;
        }

        if (std.mem.endsWith(u8, last_part, ".doxa")) {
            name = last_part[0 .. last_part.len - 5];
        } else {
            name = last_part;
        }

        if (self.debug_enabled) {
            std.debug.print("Using module name from path: {s}\n", .{name});
        }

        // Initialize symbol table for this module if needed
        if (self.imported_symbols == null) {
            self.imported_symbols = std.StringHashMap(import_parser.ImportedSymbol).init(self.allocator);
        }

        // If the module AST is a block, process its statements
        if (module_ast.* == .Block) {
            const statements = module_ast.Block.statements;
            if (self.debug_enabled) {
                std.debug.print("Processing {d} statements\n", .{statements.len});
            }

            for (statements, 0..) |stmt, i| {
                if (self.debug_enabled) {
                    std.debug.print("\nProcessing statement {d}: {s}\n", .{ i, @tagName(stmt) });
                    switch (stmt) {
                        .VarDecl => |v| std.debug.print("  Variable: {s} (public: {any})\n", .{ v.name.lexeme, v.is_public }),
                        .Function => |f| std.debug.print("  Function: {s} (public: {any})\n", .{ f.name.lexeme, f.is_public }),
                        .Module => |m| std.debug.print("  Module: {s}\n", .{m.name.lexeme}),
                        else => std.debug.print("  Other statement type\n", .{}),
                    }
                }

                switch (stmt) {
                    .Module => |module| {
                        // If we find a module declaration, use its name instead
                        name = module.name.lexeme;
                        if (self.debug_enabled) {
                            std.debug.print("Found module declaration, using name: {s}\n", .{name});
                        }
                        for (module.imports) |import| {
                            try imports.append(import);
                        }
                    },
                    .VarDecl => |var_decl| {
                        const is_public = var_decl.is_public;
                        if (self.debug_enabled) {
                            std.debug.print("Found variable declaration: {s} (public: {any})\n", .{ var_decl.name.lexeme, is_public });
                            std.debug.print("  Type info: {s}\n", .{@tagName(var_decl.type_info.base)});
                            if (var_decl.initializer) |init_expr| {
                                std.debug.print("  Has initializer: {s}\n", .{@tagName(init_expr.*)});
                            }
                        }

                        if (is_public) {
                            const full_name = try std.fmt.allocPrint(self.allocator, "{s}.{s}", .{ name, var_decl.name.lexeme });

                            if (self.debug_enabled) {
                                std.debug.print("Registering public variable: {s}\n", .{full_name});
                                std.debug.print("Checking specific_symbol condition: specific={?s}, name={s}\n", .{ specific_symbol, var_decl.name.lexeme });
                            }

                            // Only register if we want all symbols or this specific one
                            if (specific_symbol == null or std.mem.eql(u8, specific_symbol.?, var_decl.name.lexeme)) {
                                if (self.debug_enabled) {
                                    std.debug.print("Adding variable to imported symbols\n", .{});
                                }

                                try self.imported_symbols.?.put(full_name, .{
                                    .kind = .Variable,
                                    .name = var_decl.name.lexeme,
                                    .original_module = module_path,
                                });

                                if (self.debug_enabled) {
                                    std.debug.print("Successfully registered variable {s}\n", .{full_name});
                                }
                            } else if (self.debug_enabled) {
                                std.debug.print("Skipping variable registration - doesn't match specific symbol\n", .{});
                            }
                        }
                    },
                    .Function => |func| {
                        const is_public = func.is_public;
                        if (self.debug_enabled) {
                            std.debug.print("Found function declaration: {s} (public: {any})\n", .{ func.name.lexeme, is_public });
                            std.debug.print("  Return type: {s}\n", .{@tagName(func.return_type_info.base)});
                            std.debug.print("  Parameters: {d}\n", .{func.params.len});
                        }

                        if (is_public) {
                            const full_name = try std.fmt.allocPrint(self.allocator, "{s}.{s}", .{ name, func.name.lexeme });

                            if (self.debug_enabled) {
                                std.debug.print("Registering public function: {s}\n", .{full_name});
                                std.debug.print("Checking specific_symbol condition: specific={?s}, name={s}\n", .{ specific_symbol, func.name.lexeme });
                            }

                            if (specific_symbol == null or std.mem.eql(u8, specific_symbol.?, func.name.lexeme)) {
                                try self.imported_symbols.?.put(full_name, .{
                                    .kind = .Function,
                                    .name = func.name.lexeme,
                                    .original_module = module_path,
                                });

                                if (self.debug_enabled) {
                                    std.debug.print("Successfully registered function {s}\n", .{full_name});
                                }
                            } else if (self.debug_enabled) {
                                std.debug.print("Skipping function registration - doesn't match specific symbol\n", .{});
                            }
                        }
                    },
                    .Expression => |expr_opt| {
                        if (expr_opt) |expr| {
                            if (expr.* == .StructDecl) {
                                const struct_decl = expr.StructDecl;
                                const is_public = struct_decl.is_public;
                                if (self.debug_enabled) {
                                    std.debug.print("Found struct declaration: {s} (public: {any})\n", .{ struct_decl.name.lexeme, is_public });
                                }

                                if (is_public) {
                                    const full_name = try std.fmt.allocPrint(self.allocator, "{s}.{s}", .{ name, struct_decl.name.lexeme });

                                    if (self.debug_enabled) {
                                        std.debug.print("Registering public struct: {s}\n", .{full_name});
                                        std.debug.print("Checking specific_symbol condition: specific={?s}, name={s}\n", .{ specific_symbol, struct_decl.name.lexeme });
                                    }

                                    if (specific_symbol == null or std.mem.eql(u8, specific_symbol.?, struct_decl.name.lexeme)) {
                                        try self.imported_symbols.?.put(full_name, .{
                                            .kind = .Struct,
                                            .name = struct_decl.name.lexeme,
                                            .original_module = module_path,
                                        });

                                        if (self.debug_enabled) {
                                            std.debug.print("Successfully registered struct {s}\n", .{full_name});
                                        }
                                    } else if (self.debug_enabled) {
                                        std.debug.print("Skipping struct registration - doesn't match specific symbol\n", .{});
                                    }
                                }
                            }
                        }
                    },

                    else => {}, // Skip other types of statements
                }
            }
        }

        if (self.debug_enabled) {
            std.debug.print("\n=== Module info extraction complete ===\n", .{});
            std.debug.print("Module name: {s}\n", .{name});
            std.debug.print("Number of imports: {d}\n", .{imports.items.len});
            std.debug.print("Current imported symbols:\n", .{});
            if (self.imported_symbols) |symbols| {
                var it = symbols.iterator();
                while (it.next()) |entry| {
                    std.debug.print("  {s} (kind: {s})\n", .{ entry.key_ptr.*, @tagName(entry.value_ptr.kind) });
                }
            }
        }

        return ast.ModuleInfo{
            .name = name,
            .imports = try imports.toOwnedSlice(),
            .ast = module_ast, // Store the full module AST for later reference
        };
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
        pop_expr.* = .{ .ArrayPop = .{
            .array = array.?,
        } };

        return pop_expr;
    }

    pub fn arrayConcat(self: *Parser, array: ?*ast.Expr, _: Precedence) ErrorList!?*ast.Expr {
        if (array == null) return error.ExpectedExpression;

        // Parse the second array expression
        const array2 = try expression_parser.parseExpression(self) orelse return error.ExpectedExpression;

        // Don't consume the right paren here - let fieldAccess handle it
        const concat_expr = try self.allocator.create(ast.Expr);
        concat_expr.* = .{ .ArrayConcat = .{
            .array = array.?,
            .array2 = array2,
        } };
        return concat_expr;
    }

    pub fn arrayLength(self: *Parser, array: ?*ast.Expr, _: Precedence) ErrorList!?*ast.Expr {
        if (array == null) return error.ExpectedExpression;

        // Create the array length expression
        const length_expr = try self.allocator.create(ast.Expr);
        length_expr.* = .{ .ArrayLength = .{
            .array = array.?,
        } };

        return length_expr;
    }

    pub fn input(self: *Parser, _: ?*ast.Expr, _: Precedence) ErrorList!?*ast.Expr {
        if (self.debug_enabled) {
            std.debug.print("\nParsing input expression...\n", .{});
        }

        self.advance(); // consume 'input' token

        // Check if there's a prompt string
        var prompt: token.Token = undefined;
        if (self.peek().type == .STRING) {
            prompt = self.peek();
            if (self.debug_enabled) {
                std.debug.print("Found prompt string: '{s}'\n", .{prompt.lexeme});
            }
            self.advance(); // consume the string
        } else {
            // Create an empty prompt if none provided
            prompt = token.Token{
                .type = .STRING,
                .lexeme = "",
                .literal = .{ .string = "" },
                .line = self.peek().line,
                .column = self.peek().column,
            };
        }

        // Create the input expression
        const input_expr = try self.allocator.create(ast.Expr);
        input_expr.* = .{ .Input = .{
            .prompt = prompt,
        } };

        return input_expr;
    }

    pub fn loadAndRegisterModule(self: *Parser, module_path: []const u8, namespace: []const u8, specific_symbol: ?[]const u8) ErrorList!void {
        if (self.debug_enabled) {
            std.debug.print("\n=== Starting module loading and registration ===\n", .{});
            std.debug.print("Module path: {s}\n", .{module_path});
            std.debug.print("Namespace: {s}\n", .{namespace});
            std.debug.print("Specific symbol: {?s}\n", .{specific_symbol});
        }

        // Get module info - this will parse the module if not already cached
        const module_info = try self.resolveModule(module_path);

        if (self.debug_enabled) {
            std.debug.print("\nModule info resolved:\n", .{});
            std.debug.print("Module name: {s}\n", .{module_info.name});
            std.debug.print("Has AST: {any}\n", .{module_info.ast != null});
            if (module_info.ast) |module_ast| {
                std.debug.print("AST type: {s}\n", .{@tagName(module_ast.*)});
                if (module_ast.* == .Block) {
                    std.debug.print("Block has {d} statements\n", .{module_ast.Block.statements.len});
                }
            }
        }

        // Register the module in namespaces map
        try self.module_namespaces.put(namespace, module_info);

        if (self.debug_enabled) {
            std.debug.print("\nRegistered module in namespaces map\n", .{});
            std.debug.print("Current namespaces:\n", .{});
            var it = self.module_namespaces.iterator();
            while (it.next()) |entry| {
                std.debug.print("  {s}\n", .{entry.key_ptr.*});
            }
        }

        // If we have the full module AST available (from module_info),
        // we can scan it for public symbols and register them
        if (module_info.ast) |module_ast| {
            if (self.debug_enabled) {
                std.debug.print("\nScanning module AST for public symbols...\n", .{});
            }
            try self.registerPublicSymbols(module_ast, module_path, namespace, specific_symbol);
        } else if (self.debug_enabled) {
            std.debug.print("\nNo AST available for module, skipping symbol registration\n", .{});
        }

        if (self.debug_enabled) {
            std.debug.print("\n=== Module loading and registration complete ===\n", .{});
        }
    }

    // Function to scan a module's AST and register its public symbols
    fn registerPublicSymbols(self: *Parser, module_ast: *ast.Expr, module_path: []const u8, namespace: []const u8, specific_symbol: ?[]const u8) !void {
        // Initialize symbol table for this module if needed
        if (self.imported_symbols == null) {
            self.imported_symbols = std.StringHashMap(import_parser.ImportedSymbol).init(self.allocator);
        }

        if (self.debug_enabled) {
            std.debug.print("\n=== Starting symbol registration ===\n", .{});
            std.debug.print("Module path: {s}\n", .{module_path});
            std.debug.print("Namespace: {s}\n", .{namespace});
            std.debug.print("Specific symbol: {?s}\n", .{specific_symbol});
            std.debug.print("Module AST type: {s}\n", .{@tagName(module_ast.*)});
        }

        // If this is a block, process its statements
        if (module_ast.* == .Block) {
            const statements = module_ast.Block.statements;
            if (self.debug_enabled) {
                std.debug.print("Processing {d} statements\n", .{statements.len});
            }

            for (statements, 0..) |stmt, i| {
                if (self.debug_enabled) {
                    std.debug.print("\nProcessing statement {d}: {s}\n", .{ i, @tagName(stmt) });
                    switch (stmt) {
                        .VarDecl => |v| std.debug.print("  Variable: {s} (public: {any})\n", .{ v.name.lexeme, v.is_public }),
                        .Function => |f| std.debug.print("  Function: {s} (public: {any})\n", .{ f.name.lexeme, f.is_public }),
                        .Module => |m| std.debug.print("  Module: {s}\n", .{m.name.lexeme}),
                        else => std.debug.print("  Other statement type\n", .{}),
                    }
                }

                switch (stmt) {
                    // Handle enum declarations
                    .EnumDecl => |enum_decl| {
                        const is_public = enum_decl.is_public;
                        if (self.debug_enabled) {
                            std.debug.print("Found enum declaration: {s} (public: {any})\n", .{ enum_decl.name.lexeme, is_public });
                        }

                        if (is_public) {
                            const full_name = try std.fmt.allocPrint(self.allocator, "{s}.{s}", .{ namespace, enum_decl.name.lexeme });

                            if (self.debug_enabled) {
                                std.debug.print("Registering public enum: {s}\n", .{full_name});
                                std.debug.print("Checking specific_symbol condition: specific={?s}, name={s}\n", .{ specific_symbol, enum_decl.name.lexeme });
                            }

                            // Only register if we want all symbols or this specific one
                            if (specific_symbol == null or std.mem.eql(u8, specific_symbol.?, enum_decl.name.lexeme)) {
                                try self.imported_symbols.?.put(full_name, .{
                                    .kind = .Enum,
                                    .name = enum_decl.name.lexeme,
                                    .original_module = module_path,
                                });

                                // Also register each enum variant for easy access
                                for (enum_decl.variants) |variant| {
                                    const variant_full_name = try std.fmt.allocPrint(self.allocator, "{s}.{s}", .{ full_name, variant.lexeme });
                                    try self.imported_symbols.?.put(variant_full_name, .{
                                        .kind = .Enum,
                                        .name = variant.lexeme,
                                        .original_module = module_path,
                                    });
                                }

                                if (self.debug_enabled) {
                                    std.debug.print("Successfully registered enum {s}\n", .{full_name});
                                }
                            } else if (self.debug_enabled) {
                                std.debug.print("Skipping enum registration - doesn't match specific symbol\n", .{});
                            }
                        }
                    },
                    // Handle function declarations
                    .Function => |func| {
                        const is_public = func.is_public;
                        if (self.debug_enabled) {
                            std.debug.print("Found function declaration: {s} (public: {any})\n", .{ func.name.lexeme, is_public });
                        }

                        if (is_public) {
                            const full_name = try std.fmt.allocPrint(self.allocator, "{s}.{s}", .{ namespace, func.name.lexeme });

                            if (self.debug_enabled) {
                                std.debug.print("Registering public function: {s}\n", .{full_name});
                                std.debug.print("Checking specific_symbol condition: specific={?s}, name={s}\n", .{ specific_symbol, func.name.lexeme });
                            }

                            if (specific_symbol == null or std.mem.eql(u8, specific_symbol.?, func.name.lexeme)) {
                                try self.imported_symbols.?.put(full_name, .{
                                    .kind = .Function,
                                    .name = func.name.lexeme,
                                    .original_module = module_path,
                                });

                                if (self.debug_enabled) {
                                    std.debug.print("Successfully registered function {s}\n", .{full_name});
                                }
                            } else if (self.debug_enabled) {
                                std.debug.print("Skipping function registration - doesn't match specific symbol\n", .{});
                            }
                        }
                    },
                    // Handle struct declarations
                    .Expression => |expr_opt| {
                        if (expr_opt) |expr| {
                            if (expr.* == .StructDecl) {
                                const struct_decl = expr.StructDecl;
                                const is_public = struct_decl.is_public;
                                if (self.debug_enabled) {
                                    std.debug.print("Found struct declaration: {s} (public: {any})\n", .{ struct_decl.name.lexeme, is_public });
                                }

                                if (is_public) {
                                    const full_name = try std.fmt.allocPrint(self.allocator, "{s}.{s}", .{ namespace, struct_decl.name.lexeme });

                                    if (self.debug_enabled) {
                                        std.debug.print("Registering public struct: {s}\n", .{full_name});
                                        std.debug.print("Checking specific_symbol condition: specific={?s}, name={s}\n", .{ specific_symbol, struct_decl.name.lexeme });
                                    }

                                    if (specific_symbol == null or std.mem.eql(u8, specific_symbol.?, struct_decl.name.lexeme)) {
                                        try self.imported_symbols.?.put(full_name, .{
                                            .kind = .Struct,
                                            .name = struct_decl.name.lexeme,
                                            .original_module = module_path,
                                        });

                                        if (self.debug_enabled) {
                                            std.debug.print("Successfully registered struct {s}\n", .{full_name});
                                        }
                                    } else if (self.debug_enabled) {
                                        std.debug.print("Skipping struct registration - doesn't match specific symbol\n", .{});
                                    }
                                }
                            }
                        }
                    },
                    .VarDecl => |var_decl| {
                        const is_public = var_decl.is_public;
                        if (self.debug_enabled) {
                            std.debug.print("Found variable declaration: {s} (public: {any})\n", .{ var_decl.name.lexeme, is_public });
                        }

                        if (is_public) {
                            const full_name = try std.fmt.allocPrint(self.allocator, "{s}.{s}", .{ namespace, var_decl.name.lexeme });

                            if (self.debug_enabled) {
                                std.debug.print("Registering public variable: {s}\n", .{full_name});
                                std.debug.print("Checking specific_symbol condition: specific={?s}, name={s}\n", .{ specific_symbol, var_decl.name.lexeme });
                            }

                            // Only register if we want all symbols or this specific one
                            if (specific_symbol == null or std.mem.eql(u8, specific_symbol.?, var_decl.name.lexeme)) {
                                if (self.debug_enabled) {
                                    std.debug.print("Adding variable to imported symbols\n", .{});
                                }

                                try self.imported_symbols.?.put(full_name, .{
                                    .kind = .Variable,
                                    .name = var_decl.name.lexeme,
                                    .original_module = module_path,
                                });

                                if (self.debug_enabled) {
                                    std.debug.print("Successfully registered variable {s}\n", .{full_name});
                                }
                            } else if (self.debug_enabled) {
                                std.debug.print("Skipping variable registration - doesn't match specific symbol\n", .{});
                            }
                        }
                    },
                    else => {}, // Skip other types of statements
                }
            }
        }

        if (self.debug_enabled) {
            std.debug.print("\n=== Symbol registration complete ===\n", .{});
            std.debug.print("Current imported symbols:\n", .{});
            if (self.imported_symbols) |symbols| {
                var it = symbols.iterator();
                while (it.next()) |entry| {
                    std.debug.print("  {s} (kind: {s})\n", .{ entry.key_ptr.*, @tagName(entry.value_ptr.kind) });
                }
            }
        }
    }

    // Helper to check if a symbol is imported and from which module
    pub fn findImportedSymbol(self: *Parser, name: []const u8) ?import_parser.ImportedSymbol {
        if (self.imported_symbols) |symbols| {
            return symbols.get(name);
        }
        return null;
    }
};
