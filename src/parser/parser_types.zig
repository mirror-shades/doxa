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
pub const Mode = enum { Safe, Normal };

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
    mode: Mode = .Normal,
    current_file: []const u8,

    // tracking style conflicts
    fn_style: TokenStyle = .Undefined,
    assign_style: TokenStyle = .Undefined,
    equality_style: TokenStyle = .Undefined,
    and_style: TokenStyle = .Undefined,
    or_style: TokenStyle = .Undefined,
    where_style: TokenStyle = .Undefined,

    fn_style_warning_shown: bool = false,
    assign_style_warning_shown: bool = false,
    equality_style_warning_shown: bool = false,
    and_style_warning_shown: bool = false,
    or_style_warning_shown: bool = false,
    where_style_warning_shown: bool = false,

    // Add these fields to track entry points
    has_entry_point: bool = false,
    entry_point_location: ?token.Token = null,

    current_module: ?ModuleInfo = null,
    module_cache: std.StringHashMap(ModuleInfo),

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

        // Check for alternate tokens before advancing
        const current = self.peek();
        if (isAlternateToken(current.type)) {
            if (self.debug_enabled) {
                std.debug.print("Checking alternate token: {s}\n", .{@tagName(current.type)});
            }
            self.updateTokenStyle(current.type);
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
            // Special handling for return statements
            if (self.peek().type == .RETURN) {
                const return_stmt = try statement_parser.parseReturnStmt(self);
                try statements.append(return_stmt);
                break; // Exit after return statement
            }

            // Parse regular statement
            const stmt = try statement_parser.parseExpressionStmt(self);

            // Only append non-null expression statements
            switch (stmt) {
                .Expression => |expr| {
                    if (expr != null) {
                        try statements.append(stmt);
                    }
                },
                else => try statements.append(stmt),
            }

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

        // First pass: collect all function declarations and create them
        var function_table = std.StringHashMap(*ast.Stmt).init(self.allocator);
        defer function_table.deinit();

        // Store current position
        const original_pos = self.current;

        // First pass to find and store all function declarations
        while (self.peek().type != .EOF) {
            if (self.peek().type == .FN_KEYWORD or
                self.peek().type == .FUNCTION_KEYWORD)
            {
                // Don't advance here - let parseFunctionDecl handle it
                const fn_stmt = try declaration_parser.parseFunctionDecl(self);
                const fn_ptr = try self.allocator.create(ast.Stmt);
                fn_ptr.* = fn_stmt;

                // Get the function name from the statement
                if (fn_stmt == .Function) {
                    try function_table.put(fn_stmt.Function.name.lexeme, fn_ptr);
                }
            }
            self.advance();
        }

        // Reset position for main parse
        self.current = original_pos;

        // Create statements array with function declarations first
        var statements = std.ArrayList(ast.Stmt).init(self.allocator);
        errdefer {
            for (statements.items) |*stmt| {
                stmt.deinit(self.allocator);
            }
            statements.deinit();
        }

        // Add all function declarations first
        var it = function_table.iterator();
        while (it.next()) |entry| {
            try statements.append(entry.value_ptr.*.*);
        }

        // Now parse the rest of the statements normally
        try self.parseDirective();

        while (true) {
            const current = self.peek();
            if (current.type == .EOF) break;

            const stmt = blk: {
                if (self.peek().type == .VAR or self.peek().type == .CONST)
                    break :blk try declaration_parser.parseVarDecl(self)
                else if (self.peek().type == .LEFT_BRACE) {
                    const block_stmt = if (try self.block(null, .NONE)) |expr|
                        ast.Stmt{ .Expression = expr }
                    else
                        ast.Stmt{ .Expression = null };
                    break :blk block_stmt;
                } else if (self.peek().type == .FN_KEYWORD or
                    self.peek().type == .FUNCTION_KEYWORD or
                    self.peek().type == .MAIN)
                {
                    // Skip the entire function declaration
                    var brace_count: usize = 0;
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
                } else break :blk try statement_parser.parseExpressionStmt(self);
            };

            // Only append non-null expression statements
            switch (stmt) {
                .Expression => |expr| {
                    if (expr != null) {
                        try statements.append(stmt);
                    }
                },
                else => try statements.append(stmt),
            }
        }

        // After parsing everything, check if we need an entry point in safe mode
        if (self.mode == .Safe and !self.has_entry_point) {
            return error.MissingEntryPoint;
        }

        return statements.toOwnedSlice();
    }

    pub fn parseParameters(self: *Parser, params: *std.ArrayList(ast.FunctionParam)) ErrorList!void {
        if (self.debug_enabled) {
            std.debug.print("Parsing function parameters...\n", .{});
        }

        while (true) {
            if (self.peek().type != .IDENTIFIER) {
                return error.ExpectedIdentifier;
            }

            const param_name = self.peek();
            self.advance();

            // Parse type annotation if present
            var type_expr: ?*ast.TypeExpr = null;
            if (self.peek().type == .TYPE_SYMBOL) {
                self.advance(); // consume ::
                type_expr = try expression_parser.parseTypeExpr(self);
            }

            // Handle default value
            var default_value: ?*ast.Expr = null;
            if (self.peek().type == .ASSIGN_SYMBOL or self.peek().type == .ASSIGN_KEYWORD) {
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
            if (self.peek().type != .ASSIGN_SYMBOL and self.peek().type != .ASSIGN_KEYWORD) {
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
        if (self.peek().type == .ASSIGN_SYMBOL or self.peek().type == .ASSIGN_KEYWORD) {
            self.advance(); // consume =
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

    pub fn fieldAccess(self: *Parser, object: ?*ast.Expr, _: Precedence) ErrorList!?*ast.Expr {
        if (object == null) return error.ExpectedExpression;

        // The dot has already been consumed by the time we get here
        // Don't advance again

        // Parse the field name
        if (self.peek().type != .IDENTIFIER) {
            return error.ExpectedIdentifier;
        }
        const field = self.peek();
        self.advance();

        const field_access = try self.allocator.create(ast.Expr);
        field_access.* = .{ .FieldAccess = .{
            .object = object.?,
            .field = field,
        } };
        return field_access;
    }

    pub fn print(self: *Parser, left: ?*ast.Expr, _: Precedence) ErrorList!?*ast.Expr {
        if (left == null) return error.ExpectedExpression;

        const print_expr = try self.allocator.create(ast.Expr);
        print_expr.* = .{
            .Print = .{
                .expr = left.?,
                .location = .{
                    .file = self.current_file,
                    // I have no idea why this is necessary, but it is.
                    .line = @divTrunc(self.peek().line + 1, 2),
                    .column = self.peek().column,
                },
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
            if (self.peek().type != .WHERE_SYMBOL) {
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

    fn updateTokenStyle(self: *Parser, token_type: token.TokenType) void {
        // Runtime style determination
        var new_style: TokenStyle = .Undefined;
        if (token_type == .FUNCTION_KEYWORD or token_type == .AND_KEYWORD or token_type == .OR_KEYWORD or token_type == .ASSIGN_KEYWORD or token_type == .EQUALITY_KEYWORD or token_type == .WHERE_KEYWORD) {
            new_style = .Keyword;
        } else if (token_type == .FN_KEYWORD or token_type == .AND_SYMBOL or token_type == .OR_SYMBOL or token_type == .ASSIGN_SYMBOL or token_type == .EQUALITY_SYMBOL or token_type == .WHERE_SYMBOL) {
            new_style = .Symbol;
        } else {
            return;
        }
        if (self.debug_enabled) {
            std.debug.print("Updating token style for: {s}\n", .{@tagName(token_type)});
        }
        // Check and update styles
        switch (token_type) {
            .WHERE_KEYWORD, .WHERE_SYMBOL => {
                if (self.where_style != .Undefined and self.where_style != new_style and !self.where_style_warning_shown) {
                    self.reportWarning("Style conflict for 'where' keyword and '|' symbol");
                    self.where_style_warning_shown = true;
                    return;
                }
                if (self.where_style == .Undefined) {
                    self.where_style = new_style;
                }
            },
            .ASSIGN_KEYWORD, .ASSIGN_SYMBOL => {
                if (self.assign_style != .Undefined and self.assign_style != new_style and !self.assign_style_warning_shown) {
                    self.reportWarning("Style conflict for 'assignment' keyword and '=' symbol");
                    self.assign_style_warning_shown = true;
                    return;
                }
                if (self.assign_style == .Undefined) {
                    self.assign_style = new_style;
                }
            },
            .EQUALITY_KEYWORD, .EQUALITY_SYMBOL => {
                if (self.equality_style != .Undefined and self.equality_style != new_style and !self.equality_style_warning_shown) {
                    self.reportWarning("Style conflict for 'equality' keyword and '==' symbol");
                    self.equality_style_warning_shown = true;
                    return;
                }
                if (self.equality_style == .Undefined) {
                    self.equality_style = new_style;
                }
            },
            .FN_KEYWORD, .FUNCTION_KEYWORD => {
                if (self.fn_style != .Undefined and self.fn_style != new_style and !self.fn_style_warning_shown) {
                    self.reportWarning("Style conflict for 'fn' and 'function' keywords");
                    self.fn_style_warning_shown = true;
                    return;
                }
                if (self.fn_style == .Undefined) {
                    self.fn_style = new_style;
                }
            },
            .AND_KEYWORD, .AND_SYMBOL => {
                if (self.and_style != .Undefined and self.and_style != new_style and !self.and_style_warning_shown) {
                    self.reportWarning("Style conflict for 'and' keyword and '&&' symbol");
                    self.and_style_warning_shown = true;
                    return;
                }
                if (self.and_style == .Undefined) {
                    self.and_style = new_style;
                }
            },
            .OR_KEYWORD, .OR_SYMBOL => {
                if (self.or_style != .Undefined and self.or_style != new_style and !self.or_style_warning_shown) {
                    self.reportWarning("Style conflict for 'or' keyword and '||' symbol");
                    self.or_style_warning_shown = true;
                    return;
                }
                if (self.or_style == .Undefined) {
                    self.or_style = new_style;
                }
            },
            else => {},
        }
    }

    fn parseDirective(self: *Parser) ErrorList!void {
        const current = self.peek();
        if (current.type == .HASH) {
            self.advance(); // consume #
            if (self.peek().type == .IDENTIFIER) {
                // if identifer, do something for custom doxas, ect. ect.
                // this wont be added for a long time so don't wait around
            }
            if (self.peek().type == .SAFE) {
                self.mode = .Safe;
                if (self.debug_enabled) {
                    std.debug.print("Safe mode enabled\n", .{});
                }
                self.advance(); // consume directive name
                return;
            }
            return error.InvalidDirective;
        }
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

        // Create a block expression to hold the module statements
        const module_block = try self.allocator.create(ast.Expr);
        module_block.* = .{ .Block = .{
            .statements = try new_parser.execute(),
            .value = null,
        } };

        // Extract module info and cache it
        const info = try self.extractModuleInfo(module_block);
        try self.module_cache.put(module_name, info);

        return info;
    }

    pub fn loadModuleSource(self: *Parser, module_name: []const u8) ErrorList![]const u8 {
        // Construct the module path
        // Assuming modules are in a "modules" directory relative to the current file
        var path_buffer = std.ArrayList(u8).init(self.allocator);
        defer path_buffer.deinit();

        // Get the directory of the current file
        const current_dir = std.fs.path.dirname(self.current_file) orelse ".";
        try path_buffer.appendSlice(current_dir);
        try path_buffer.appendSlice("/modules/");
        try path_buffer.appendSlice(module_name);
        try path_buffer.appendSlice(".dx"); // Assuming .dx is your file extension

        // Read the file
        const file = try std.fs.openFileAbsolute(path_buffer.items, .{});
        defer file.close();

        // Get file size
        const file_size = try file.getEndPos();

        // Allocate buffer and read file
        const buffer = try self.allocator.alloc(u8, file_size);
        const bytes_read = try file.readAll(buffer);
        if (bytes_read != file_size) {
            self.allocator.free(buffer);
            return error.IncompleteRead;
        }

        return buffer;
    }

    pub fn extractModuleInfo(self: *Parser, module_ast: *ast.Expr) ErrorList!ast.ModuleInfo {
        var imports = std.ArrayList(ast.ImportInfo).init(self.allocator);
        var mode: ast.ModuleMode = .Normal;
        var name: []const u8 = "";

        // If the module AST is a block, process its statements
        if (module_ast.* == .Block) {
            const statements = module_ast.Block.statements;
            for (statements) |stmt| {
                switch (stmt) {
                    .Module => |module| {
                        name = module.name.lexeme;
                        mode = if (module.is_safe) .Safe else .Normal;
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
            .mode = mode,
            .imports = try imports.toOwnedSlice(),
        };
    }

    /// Returns the previous token in the token list
    pub fn previous(self: *Parser) token.Token {
        if (self.current == 0) {
            return self.tokens[0];
        }
        return self.tokens[self.current - 1];
    }
};
