const std = @import("std");
const ast = @import("../ast.zig");
const ModuleInfo = @import("../ast.zig").ModuleInfo;
const token = @import("../token.zig");
const declaration_parser = @import("declaration_parser.zig");
const expression_parser = @import("expression_parser.zig");
const statement_parser = @import("statement_parser.zig");
const ErrorList = @import("../reporting.zig").ErrorList;
const Precedence = @import("./precedence.zig").Precedence;
const Reporting = @import("../reporting.zig").Reporting;
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
        if (self.debug_enabled) {
            std.debug.print("Advancing from position {} to {}\n", .{
                self.current,
                self.current + 1,
            });
        }

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
        if (self.debug_enabled) {
            std.debug.print("\nToken stream:\n", .{});
            for (self.tokens, 0..) |t, i| {
                std.debug.print("{}: {s} ({s})\n", .{ i, @tagName(t.type), t.lexeme });
            }
            std.debug.print("\n", .{});
        }

        // First pass: process all imports so that their symbols are available for the rest
        var imports = std.ArrayList(ast.ImportInfo).init(self.allocator);
        defer imports.deinit();

        // Store current position
        const original_pos = self.current;

        // Process imports first
        while (self.peek().type != .EOF) {
            if (self.peek().type == .IMPORT) {
                const import_stmt = try import_parser.parseImportStmt(self);
                try imports.append(import_stmt.Import);
            } else {
                self.advance();
            }
        }

        // Reset position for second pass
        self.current = original_pos;

        // Second pass: collect all function declarations and create them
        var forward_declarations = std.StringHashMap(*ast.Stmt).init(self.allocator);
        defer forward_declarations.deinit();

        while (self.peek().type != .EOF) {
            // Check for entry point token (->)
            var is_entry = false;
            if (self.peek().type == .MAIN) {
                is_entry = true;
                self.has_entry_point = true;
                self.entry_point_location = self.peek();
                self.advance(); // consume ->
            }

            // Check for public modifier
            var is_public = false;
            if (self.peek().type == .PUBLIC) {
                is_public = true;
                self.advance(); // consume pub/public
            }

            if (self.peek().type == .FUNCTION) {
                // Don't advance here - let parseFunctionDecl handle it
                const fn_stmt = try declaration_parser.parseFunctionDecl(self);
                const fn_ptr = try self.allocator.create(ast.Stmt);
                fn_ptr.* = fn_stmt;

                // Set the public flag if this was a public function
                if (fn_stmt == .Function and is_public) {
                    fn_ptr.*.Function.is_public = true;
                }

                // Get the function name from the statement
                if (fn_stmt == .Function) {
                    // Store function name as entry point if this is an entry point
                    if (is_entry) {
                        self.entry_point_name = fn_stmt.Function.name.lexeme;
                        // Update the function structure to mark it as an entry point
                        fn_ptr.*.Function.is_entry = true;
                    }
                    try forward_declarations.put(fn_stmt.Function.name.lexeme, fn_ptr);
                }
            } else if (self.peek().type == .VAR or self.peek().type == .CONST) {
                const var_stmt = try declaration_parser.parseVarDecl(self);
                const var_ptr = try self.allocator.create(ast.Stmt);
                var_ptr.* = var_stmt;
                try forward_declarations.put(var_stmt.VarDecl.name.lexeme, var_ptr);
            } else {
                self.advance(); // Only advance if we didn't parse a function
            }
        }

        // Create statements array with function declarations first
        var statements = std.ArrayList(ast.Stmt).init(self.allocator);
        errdefer {
            for (statements.items) |*stmt| {
                stmt.deinit(self.allocator);
            }
            statements.deinit();
        }

        // Add function declarations from forward_declarations - keeping them at the beginning is useful
        // for function forward references
        var it = forward_declarations.iterator();
        while (it.next()) |entry| {
            if (entry.value_ptr.*.* == .Function) {
                try statements.append(entry.value_ptr.*.*);
            }
        }

        // Reset position to start of file to process statements in source order
        self.current = 0;

        // Process statements in source order
        while (self.current < self.tokens.len) {
            const current = self.peek();
            if (current.type == .EOF) break;

            const stmt = blk: {
                if (self.peek().type == .IMPORT) {
                    // We've already processed imports, so just skip this statement
                    const import_stmt = try import_parser.parseImportStmt(self);
                    break :blk ast.Stmt{ .Import = import_stmt.Import };
                } else if (self.peek().type == .VAR or self.peek().type == .CONST)
                    break :blk try declaration_parser.parseVarDecl(self)
                else if (self.peek().type == .LEFT_BRACE) {
                    const block_stmt = if (try self.block(null, .NONE)) |expr|
                        ast.Stmt{ .Expression = expr }
                    else
                        ast.Stmt{ .Expression = null };
                    break :blk block_stmt;
                } else if (self.peek().type == .FUNCTION or
                    self.peek().type == .MAIN or
                    self.peek().type == .PUBLIC)
                {
                    // We already have the function declarations in forward_declarations
                    // Skip them since we've already processed them
                    var brace_count: usize = 0;
                    // If this is PUBLIC token, skip it first
                    if (self.peek().type == .PUBLIC) {
                        self.advance(); // Skip PUBLIC token
                    }
                    while (self.peek().type != .EOF) {
                        if (self.peek().type == .LEFT_BRACE) {
                            brace_count += 1;
                        } else if (self.peek().type == .RIGHT_BRACE) {
                            brace_count -= 1;
                            if (brace_count == 0) {
                                self.advance(); // consume the final }
                                break;
                            }
                        }
                        self.advance();
                    }
                    break :blk ast.Stmt{ .Expression = null };
                } else if (self.peek().type == .STRUCT_TYPE) {
                    // Convert the struct declaration expression to a statement
                    const expr = try declaration_parser.parseStructDecl(self, null, .NONE);
                    break :blk ast.Stmt{ .Expression = expr };
                } else if (self.peek().type == .ENUM_TYPE) {
                    break :blk try declaration_parser.parseEnumDecl(self);
                } else if (self.peek().type == .TRY) {
                    break :blk try statement_parser.parseTryStmt(self);
                } else if (self.peek().type == .IDENTIFIER) {
                    // This could be an assignment or other expression
                    break :blk try statement_parser.parseExpressionStmt(self);
                } else break :blk try statement_parser.parseExpressionStmt(self);
            };

            // Only append non-null expression statements
            switch (stmt) {
                .Expression => |expr| {
                    if (expr != null) {
                        try statements.append(stmt);
                    }
                },
                .Import => {
                    // We already processed the imports in the first pass
                    // Just skip them in the final statements list to avoid duplicates
                },
                else => try statements.append(stmt),
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
                var reporting = Reporting.init();
                reporting.reportCompileError(.{
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

        // Get the variable name if this is a variable expression
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
        const info = try self.extractModuleInfo(module_block);
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

    pub fn extractModuleInfo(self: *Parser, module_ast: *ast.Expr) ErrorList!ast.ModuleInfo {
        var imports = std.ArrayList(ast.ImportInfo).init(self.allocator);
        var name: []const u8 = "";

        // If the module AST is a block, process its statements
        if (module_ast.* == .Block) {
            const statements = module_ast.Block.statements;
            for (statements) |stmt| {
                switch (stmt) {
                    .Module => |module| {
                        name = module.name.lexeme;
                        for (module.imports) |import| {
                            try imports.append(import);
                        }
                    },
                    else => {}, // Skip other statement types
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
        // Get module info - this will parse the module if not already cached
        const module_info = try self.resolveModule(module_path);

        // Register the module in namespaces map
        try self.module_namespaces.put(namespace, module_info);

        // If we have the full module AST available (from module_info),
        // we can scan it for public symbols and register them
        if (module_info.ast) |module_ast| {
            try self.registerPublicSymbols(module_ast, module_path, namespace, specific_symbol);
        }
    }

    // Function to scan a module's AST and register its public symbols
    fn registerPublicSymbols(self: *Parser, module_ast: *ast.Expr, module_path: []const u8, namespace: []const u8, specific_symbol: ?[]const u8) !void {
        // Initialize symbol table for this module if needed
        if (self.imported_symbols == null) {
            self.imported_symbols = std.StringHashMap(import_parser.ImportedSymbol).init(self.allocator);
        }

        if (self.debug_enabled) {
            std.debug.print("Registering public symbols from module: {s}, namespace: {s}\n", .{ module_path, namespace });
        }

        // If this is a block, process its statements
        if (module_ast.* == .Block) {
            const statements = module_ast.Block.statements;
            for (statements) |stmt| {
                switch (stmt) {
                    // Handle enum declarations
                    .EnumDecl => |enum_decl| {
                        const is_public = enum_decl.is_public;
                        if (is_public) {
                            const full_name = try std.fmt.allocPrint(self.allocator, "{s}.{s}", .{ namespace, enum_decl.name.lexeme });

                            if (self.debug_enabled) {
                                std.debug.print("Registering public enum: {s}\n", .{full_name});
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
                            }
                        }
                    },
                    // Handle function declarations
                    .Function => |func| {
                        const is_public = func.is_public;
                        if (is_public) {
                            const full_name = try std.fmt.allocPrint(self.allocator, "{s}.{s}", .{ namespace, func.name.lexeme });

                            if (self.debug_enabled) {
                                std.debug.print("Registering public function: {s}\n", .{full_name});
                            }

                            if (specific_symbol == null or std.mem.eql(u8, specific_symbol.?, func.name.lexeme)) {
                                try self.imported_symbols.?.put(full_name, .{
                                    .kind = .Function,
                                    .name = func.name.lexeme,
                                    .original_module = module_path,
                                });
                            }
                        }
                    },
                    // Handle struct declarations
                    .Expression => |expr_opt| {
                        if (expr_opt) |expr| {
                            if (expr.* == .StructDecl) {
                                const struct_decl = expr.StructDecl;
                                const is_public = struct_decl.is_public;
                                if (is_public) {
                                    const full_name = try std.fmt.allocPrint(self.allocator, "{s}.{s}", .{ namespace, struct_decl.name.lexeme });

                                    if (specific_symbol == null or std.mem.eql(u8, specific_symbol.?, struct_decl.name.lexeme)) {
                                        try self.imported_symbols.?.put(full_name, .{
                                            .kind = .Struct,
                                            .name = struct_decl.name.lexeme,
                                            .original_module = module_path,
                                        });
                                    }
                                }
                            }
                        }
                    },
                    else => {}, // Skip other types of statements
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
