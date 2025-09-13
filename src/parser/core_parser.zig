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

    /// Returns the previous token in the token list
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

    // Main parsing loop - moved from parser_types.zig
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

    // Add the remaining utility functions that are still needed
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

    // Module resolution functions - moved from parser_types.zig
    pub fn loadAndRegisterModule(self: *Parser, module_path: []const u8, namespace: []const u8, specific_symbol: ?[]const u8) !void {
        _ = self;
        _ = module_path;
        _ = namespace;
        _ = specific_symbol;
        // TODO: Implement this properly by calling module_resolver functions
    }

    pub fn loadAndRegisterSpecificSymbol(self: *Parser, module_path: []const u8, symbol_name: []const u8) !void {
        _ = self;
        _ = module_path;
        _ = symbol_name;
        // TODO: Implement this properly
    }

    pub fn registerSpecificSymbol(self: *Parser, symbol_name: []const u8, symbol_info: ast.ModuleSymbol) !void {
        _ = self;
        _ = symbol_name;
        _ = symbol_info;
        // TODO: Implement this properly
    }

    pub fn recordModuleImport(self: *Parser, current_file: []const u8, namespace: []const u8, module_path: []const u8) !void {
        _ = self;
        _ = current_file;
        _ = namespace;
        _ = module_path;
        // TODO: Implement this properly
    }

    pub fn registerPublicSymbols(self: *Parser, module_info: ModuleInfo) !void {
        _ = self;
        _ = module_info;
        // TODO: Implement this properly
    }

    pub fn findImportedSymbol(self: *Parser, symbol_name: []const u8) ?ast.ModuleSymbol {
        _ = self;
        _ = symbol_name;
        // TODO: Implement this properly
        return null;
    }

    pub fn resolveModule(self: *Parser, module_path: []const u8) ![]const u8 {
        _ = self;
        // TODO: Implement this properly
        return module_path;
    }

    pub fn normalizeModulePath(self: *Parser, module_path: []const u8) ![]const u8 {
        _ = self;
        // TODO: Implement this properly
        return module_path;
    }

    pub fn reportCircularImport(self: *Parser, module_path: []const u8) void {
        _ = self;
        _ = module_path;
        // TODO: Implement this properly
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
            .public_symbols = std.ArrayList(ast.ModuleSymbol).init(self.allocator),
            .imports = std.ArrayList(ast.ImportStmt).init(self.allocator),
        };
    }

    pub fn extractModuleInfoWithParser(self: *Parser, source: []const u8, file_path: []const u8, module_name: ?[]const u8, parser: *Parser) !ModuleInfo {
        _ = source;
        _ = module_name;
        _ = parser;
        // TODO: Implement this properly
        return ModuleInfo{
            .file_path = file_path,
            .public_symbols = std.ArrayList(ast.ModuleSymbol).init(self.allocator),
            .imports = std.ArrayList(ast.ImportStmt).init(self.allocator),
        };
    }

    pub fn input(self: *Parser, left: ?*ast.Expr, _: Precedence) ErrorList!?*ast.Expr {
        _ = left; // input doesn't use left operand

        // Consume the INPUT token
        const input_token = self.peek();
        self.advance();

        // Create the input expression
        const input_expr = try self.allocator.create(ast.Expr);
        input_expr.* = .{
            .base = .{
                .id = ast.generateNodeId(),
                .span = ast.SourceSpan.fromToken(input_token),
            },
            .data = .{
                .Input = .{
                    .prompt = input_token,
                },
            },
        };

        return input_expr;
    }

    // Array operations - moved from parser_types.zig
    pub fn arrayPush(self: *Parser, array: ?*ast.Expr, _: Precedence) ErrorList!?*ast.Expr {
        _ = self;
        _ = array;
        // TODO: Implement this properly
        return null;
    }

    pub fn arrayIsEmpty(self: *Parser, array: ?*ast.Expr, _: Precedence) ErrorList!?*ast.Expr {
        _ = self;
        _ = array;
        // TODO: Implement this properly
        return null;
    }

    pub fn arrayPop(self: *Parser, array: ?*ast.Expr, _: Precedence) ErrorList!?*ast.Expr {
        _ = self;
        _ = array;
        // TODO: Implement this properly
        return null;
    }

    pub fn arrayConcat(self: *Parser, left: ?*ast.Expr, _: Precedence) ErrorList!?*ast.Expr {
        _ = self;
        _ = left;
        // TODO: Implement this properly
        return null;
    }

    pub fn arrayLength(self: *Parser, array: ?*ast.Expr, _: Precedence) ErrorList!?*ast.Expr {
        _ = self;
        _ = array;
        // TODO: Implement this properly
        return null;
    }

    // Internal call parsing - moved from parser_types.zig
    pub fn internalCallExpr(self: *Parser, left: ?*ast.Expr, precedence: Precedence) ErrorList!?*ast.Expr {
        // Delegate to the actual implementation in internal_call_parser.zig
        return internal_call_parser.internalCallExpr(self, left, precedence);
    }

    pub fn parsePrintMethod(self: *Parser) ErrorList!?*ast.Expr {
        // Delegate to the actual implementation in internal_call_parser.zig
        return internal_call_parser.parsePrintMethod(self);
    }

    // Expression parsing - moved from parser_types.zig
    pub fn block(self: *Parser, _: anytype, _: anytype) ErrorList!?*ast.Expr {
        _ = self;
        // TODO: Implement this properly
        return null;
    }

    pub fn parseStructInit(self: *Parser) ErrorList!?*ast.Expr {
        _ = self;
        // TODO: Implement this properly
        return null;
    }

    pub fn index(self: *Parser, left: ?*ast.Expr, _: Precedence) ErrorList!?*ast.Expr {
        if (left == null) return error.ExpectedLeftOperand;

        // Parse the index expression inside the brackets
        const index_expr = try expression_parser.parseExpression(self) orelse return error.ExpectedExpression;

        // Expect closing bracket
        if (self.peek().type != .RIGHT_BRACKET) return error.ExpectedRightBracket;
        self.advance(); // consume ']'

        // Create the index expression
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

        // Parse the right-hand side expression
        const right = try expression_parser.parseExpression(self) orelse return error.ExpectedExpression;

        // Create the assignment expression
        const assignment_node = try self.allocator.create(ast.Expr);
        assignment_node.* = .{
            .base = .{
                .id = ast.generateNodeId(),
                .span = ast.SourceSpan.fromToken(self.previous()),
            },
            .data = .{
                .Assignment = .{
                    .name = self.previous(), // The variable name token
                    .value = right,
                },
            },
        };

        return assignment_node;
    }

    pub fn fieldAccess(self: *Parser, left: ?*ast.Expr, _: Precedence) ErrorList!?*ast.Expr {
        if (left == null) return error.ExpectedLeftOperand;

        // Expect a field name after the dot
        if (self.peek().type != .IDENTIFIER) return error.ExpectedIdentifier;
        const field_name = self.peek();
        self.advance();

        // Check if this is an enum member access by looking at the left operand
        // For now, we'll assume it's a field access, but this could be enhanced
        // to detect enum types and create EnumMember expressions instead
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

        // Expect a field name after the dot
        if (self.peek().type != .IDENTIFIER) return error.ExpectedIdentifier;
        const member_name = self.peek();
        self.advance();

        // Create the enum member expression
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

    pub fn parseMap(self: *Parser) ErrorList!?*ast.Expr {
        _ = self;
        // TODO: Implement this properly
        return null;
    }

    pub fn parseBlock(self: *Parser) ErrorList!?*ast.Expr {
        _ = self;
        // TODO: Implement this properly
        return null;
    }

    // Additional functions referenced in precedence.zig
    pub fn print(self: *Parser, left: ?*ast.Expr, _: Precedence) ErrorList!?*ast.Expr {
        _ = self;
        _ = left;
        // TODO: Implement this properly
        return null;
    }

    // Utility functions - moved from parser_types.zig
    pub fn methodNameToTokenType(self: *Parser, method_name: []const u8) token.TokenType {
        _ = self;
        _ = method_name;
        // TODO: Implement this properly
        return .IDENTIFIER;
    }
};
