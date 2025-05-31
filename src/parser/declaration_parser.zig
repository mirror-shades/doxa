const std = @import("std");
const Parser = @import("./parser_types.zig").Parser;
const ast = @import("../ast/ast.zig");
const token = @import("../lexer/token.zig");
const Reporting = @import("../utils/reporting.zig");
const ErrorList = Reporting.ErrorList;
const Reporter = Reporting.Reporter;
const expression_parser = @import("./expression_parser.zig");
const Precedence = @import("./precedence.zig").Precedence;
const statement_parser = @import("./statement_parser.zig");
const precedence = @import("./precedence.zig");
const declaration_parser = @import("./declaration_parser.zig");

pub fn parseEnumDecl(self: *Parser) ErrorList!ast.Stmt {
    // Check for public keyword
    var is_public = false;
    if (self.peek().type == .PUBLIC) {
        is_public = true;
        self.advance(); // consume 'public'
    }

    self.advance(); // consume 'enum' keyword

    // Parse enum name
    if (self.peek().type != .IDENTIFIER) {
        return error.ExpectedIdentifier;
    }
    const name = self.peek();

    // Register the enum type before advancing
    try self.declared_types.put(name.lexeme, {});

    self.advance(); // Now advance past the identifier

    // Parse opening brace
    if (self.peek().type != .LEFT_BRACE) {
        return error.ExpectedLeftBrace;
    }
    self.advance();

    // Parse variants
    var variants = std.ArrayList(token.Token).init(self.allocator);
    errdefer variants.deinit();

    while (self.peek().type != .RIGHT_BRACE) {
        if (self.peek().type != .IDENTIFIER) {
            return error.ExpectedIdentifier;
        }
        try variants.append(self.peek());
        self.advance();

        if (self.peek().type == .COMMA) {
            self.advance();
        }
    }
    self.advance(); // consume right brace

    return ast.Stmt{ .EnumDecl = .{
        .name = name,
        .variants = try variants.toOwnedSlice(),
        .is_public = is_public,
    } };
}

pub fn parseStructDecl(self: *Parser, _: ?*ast.Expr, _: Precedence) ErrorList!?*ast.Expr {
    // Check for public keyword
    var is_public = false;
    if (self.peek().type == .PUBLIC) {
        is_public = true;
        self.advance(); // consume 'public'
    }

    // Consume 'struct' keyword
    self.advance();

    // Parse struct name
    if (self.peek().type != .IDENTIFIER) {
        return error.ExpectedIdentifier;
    }
    const name = self.peek();

    // Register the struct type name before advancing
    try self.declared_types.put(name.lexeme, {});

    self.advance(); // Now advance past the identifier

    // Expect opening brace
    if (self.peek().type != .LEFT_BRACE) {
        return error.ExpectedLeftBrace;
    }
    self.advance();

    // Parse fields
    var fields = std.ArrayList(*ast.StructField).init(self.allocator);
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

        // Expect :
        if (self.peek().type != .TYPE_SYMBOL) {
            return error.ExpectedTypeAnnotation;
        }
        self.advance();

        // Parse field type
        const type_expr = try expression_parser.parseTypeExpr(self) orelse return error.ExpectedType;

        // Create field
        const field = try self.allocator.create(ast.StructField);
        field.* = .{
            .name = field_name,
            .type_expr = type_expr,
        };
        try fields.append(field);

        // Handle field separator (comma)
        if (self.peek().type == .COMMA) {
            self.advance();
            // Allow trailing comma by checking for closing brace
            if (self.peek().type == .RIGHT_BRACE) {
                break;
            }
        } else if (self.peek().type != .RIGHT_BRACE) {
            return error.ExpectedCommaOrBrace;
        }
    }

    self.advance(); // consume right brace

    const struct_expr = try self.allocator.create(ast.Expr);
    struct_expr.* = .{ .StructDecl = .{
        .name = name,
        .fields = try fields.toOwnedSlice(),
        .is_public = is_public,
    } };

    return struct_expr;
}

pub fn parseFunctionDecl(self: *Parser) ErrorList!ast.Stmt {
    var is_entry = false;
    var is_public = false;

    // Check for public keyword
    if (self.peek().type == .PUBLIC) {
        is_public = true;
        self.advance(); // consume 'public'
    }

    // Check for entry point marker
    if (self.peek().type == .MAIN) {
        is_entry = true;

        // Check if we already have an entry point
        if (self.has_entry_point) {
            return error.MultipleEntryPoints;
        }

        self.has_entry_point = true;
        self.entry_point_location = self.peek();
        self.advance(); // consume ->
    }

    // Expect fn or function keyword
    if (self.peek().type != .FUNCTION) {
        return error.ExpectedFunction;
    }
    self.advance(); // consume fn/function keyword

    if (self.peek().type != .IDENTIFIER) {
        return error.ExpectedIdentifier;
    }

    // Store the name after parsing the IDENTIFIER token
    const function_name = self.peek();

    // If this is an entry point, store the function name
    if (is_entry) {
        self.entry_point_name = function_name.lexeme;
    }

    self.advance();

    // Parse parameter list
    if (self.peek().type != .LEFT_PAREN) {
        return error.ExpectedLeftParen;
    }
    self.advance();

    var params = std.ArrayList(ast.FunctionParam).init(self.allocator);
    errdefer {
        for (params.items) |*param| {
            param.deinit(self.allocator);
        }
        params.deinit();
    }

    if (self.peek().type != .RIGHT_PAREN) {
        try Parser.parseParameters(self, &params);
    }

    if (self.peek().type != .RIGHT_PAREN) {
        return error.ExpectedRightParen;
    }
    self.advance();

    // Parse return type
    var return_type = ast.TypeInfo{ .base = .Nothing }; // Default to nothing
    var has_return_type = false;

    if (self.peek().type == .RETURNS) {
        has_return_type = true;
        self.advance();
        if (self.peek().type == .LEFT_PAREN) {
            self.advance(); // consume (

            // Use parseTypeExpr instead of direct type parsing
            const type_expr = try expression_parser.parseTypeExpr(self) orelse return error.InvalidType;

            // Convert TypeExpr to TypeInfo
            return_type = switch (type_expr.*) {
                .Basic => |basic| ast.TypeInfo{ .base = switch (basic) {
                    .Integer => ast.Type.Int,
                    .U8 => ast.Type.U8,
                    .Float => ast.Type.Float,
                    .String => ast.Type.String,
                    .Boolean => ast.Type.Boolean,
                    .Auto => ast.Type.Auto,
                    .Tetra => ast.Type.Tetra,
                } },
                .Array => |array| blk: {
                    const element_type = try self.allocator.create(ast.TypeInfo);
                    element_type.* = .{ .base = switch (array.element_type.*) {
                        .Basic => |basic| switch (basic) {
                            .Integer => ast.Type.Int,
                            .U8 => ast.Type.U8,
                            .Float => ast.Type.Float,
                            .String => ast.Type.String,
                            .Boolean => ast.Type.Boolean,
                            .Auto => ast.Type.Auto,
                            .Tetra => ast.Type.Tetra,
                        },
                        else => return error.InvalidType,
                    } };
                    break :blk ast.TypeInfo{
                        .base = ast.Type.Array,
                        .array_type = element_type,
                    };
                },
                else => return error.InvalidType,
            };

            if (self.peek().type != .RIGHT_PAREN) {
                return error.ExpectedRightParen;
            }
            self.advance();
        } else {
            return error.ExpectedLeftParen;
        }
    }

    // unsure if I want to enforce parameter types
    // for (params.items) |param| {
    //     if (param.type_expr == null) {
    //         var reporter = Reporter.init();
    //         const location: Reporter.Location = .{ .file = self.current_file, .line = param.name.line, .column = param.name.column };
    //         reporter.reportCompileError(location, "Missing parameter type", .{});
    //         return error.MissingParameterType;
    //     }
    // }

    // 2. If function has any return statements with values, must use returns(type)
    // if ((try Parser.hasReturnWithValue(self)) and !has_return_type) {
    //     return error.MissingReturnType;
    // }

    // Parse function body
    if (self.peek().type != .LEFT_BRACE) {
        return error.ExpectedLeftBrace;
    }

    const body = try statement_parser.parseBlockStmt(self);

    return ast.Stmt{ .Function = .{
        .name = function_name,
        .params = try params.toOwnedSlice(),
        .return_type_info = return_type,
        .body = body,
        .is_entry = is_entry,
        .is_public = is_public,
    } };
}

pub fn parseVarDecl(self: *Parser) ErrorList!ast.Stmt {
    const is_public = (self.peek().type == .PUBLIC) or
        (self.previous().type == .PUBLIC and
            (self.peek().type == .VAR or self.peek().type == .CONST));
    if (is_public) {
        if (self.debug_enabled) {
            std.debug.print("Parsing public variable declaration\n", .{});
        }
    }
    var is_const = false;
    // Check if this is a var or const declaration
    if (self.peek().type == .VAR or self.peek().type == .CONST) {
        is_const = self.peek().type == .CONST;
        self.advance(); // consume 'var' or 'const'
    } else {
        return error.ExpectedVarOrConst;
    }

    if (self.debug_enabled) {
        std.debug.print("\nParsing var declaration\n", .{});
        std.debug.print("Current token: {s}\n", .{@tagName(self.peek().type)});
    }

    var type_info: ast.TypeInfo = .{
        .base = .Auto,
        .is_dynamic = true, // Start as dynamic
        .is_mutable = !is_const, // const variables can't change value
    };

    // Special handling for array type declarations
    if (self.peek().type == .ARRAY_TYPE) {
        type_info.base = .Array;
        type_info.is_dynamic = false;
        self.advance(); // consume 'array'

        // Create implicit name for array
        const name = token.Token{
            .type = .IDENTIFIER,
            .lexeme = "array",
            .literal = .{ .nothing = {} },
            .line = 0,
            .column = 0,
        };

        var initializer: ?*ast.Expr = null;
        if (self.peek().type == .ASSIGN) {
            self.advance();
            if (self.debug_enabled) {
                std.debug.print("\nParsing array initializer at position {}, token: {s}\n", .{
                    self.current,
                    @tagName(self.peek().type),
                });
            }

            initializer = try expression_parser.parseExpression(self);
            if (initializer == null) {
                var reporter = Reporter.init();
                const location = Reporter.Location{
                    .file = self.current_file,
                    .line = self.peek().line,
                    .column = self.peek().column,
                };
                reporter.reportCompileError(location, "Expected expression", .{});
                return error.ExpectedExpression;
            }
        }

        if (self.peek().type != .SEMICOLON) {
            if (self.debug_enabled) {
                std.debug.print("Expected semicolon, but found: {s}\n", .{@tagName(self.peek().type)});
            }
            var reporter = Reporter.init();
            const location = Reporter.Location{
                .file = self.current_file,
                .line = self.peek().line,
                .column = self.peek().column,
            };
            reporter.reportCompileError(location, "Expected semicolon", .{});
            return error.ExpectedSemicolon;
        }
        self.advance();

        return ast.Stmt{ .VarDecl = .{
            .name = name,
            .type_info = type_info,
            .initializer = initializer,
            .is_public = is_public,
        } };
    }

    // Original variable declaration parsing logic
    const name = if (self.peek().type != .IDENTIFIER) {
        var reporter = Reporter.init();
        const location = Reporter.Location{
            .file = self.current_file,
            .line = self.peek().line,
            .column = self.peek().column,
        };
        reporter.reportCompileError(location, "Expected identifier, got {s} \n Proper declaration: var name is 5; or, var name :: int is 5;", .{@tagName(self.peek().type)});
        return error.ExpectedIdentifier;
    } else blk: {
        const n = self.peek();
        self.advance();
        break :blk n;
    };

    if (self.debug_enabled) {
        std.debug.print("After name, current token: {s}\n", .{@tagName(self.peek().type)});
    }

    // still unsure if I should enforce explicit type annotations
    // if (self.peek().type != .TYPE_SYMBOL) {
    //     const location = Reporter.Location{
    //         .file = self.current_file,
    //         .line = self.peek().line,
    //         .column = self.peek().column,
    //     };
    //     var reporter = Reporter.init();
    //     reporter.reportCompileError(location, "Type must be declared, use auto if needed.", .{});
    //     return error.ExpectedTypeAnnotation;
    // }

    // Check specifically for '=' where 'is' or '::' is expected
    if (std.mem.eql(u8, self.peek().lexeme, "=")) {
        var reporter = Reporter.init();
        const location = Reporter.Location{
            .file = self.current_file,
            .line = self.peek().line,
            .column = self.peek().column,
        };
        reporter.reportCompileError(location, "The equal sign '=' is not used for variable declarations, use 'is' instead", .{});
        // Optionally, you could try to recover by treating '=' as 'is' here,
        // but returning an error is usually better.
        return error.UseIsForAssignment; // You might need to add this error
    }

    // Handle type annotation
    if (self.peek().type == .TYPE_SYMBOL) {
        self.advance(); // consume ::
        type_info.is_dynamic = false; // Explicitly typed variables are not dynamic
        const type_expr = try expression_parser.parseTypeExpr(self) orelse return error.ExpectedType;

        // Use the typeInfoFromExpr function to properly convert TypeExpr to TypeInfo
        const type_info_ptr = try ast.typeInfoFromExpr(self.allocator, type_expr);
        type_info = type_info_ptr.*;
        self.allocator.destroy(type_info_ptr); // Free the pointer since we copied the struct

        // Preserve mutability from our original type_info
        type_info.is_mutable = !is_const;
    }

    var initializer: ?*ast.Expr = null;

    // Check if the next token is INPUT, which is a special case
    if (self.peek().type == .INPUT) {
        if (self.debug_enabled) {
            std.debug.print("\nParsing input expression at position {}, token: {s}\n", .{
                self.current,
                @tagName(self.peek().type),
            });
        }

        // Handle input expression directly without requiring an assignment operator
        initializer = try Parser.input(self, null, .NONE);
        if (initializer == null) return error.ExpectedExpression;
    } else if (self.peek().type == .ASSIGN) {
        self.advance();
        if (self.debug_enabled) {
            std.debug.print("\nParsing var initializer at position {}, token: {s}\n", .{
                self.current,
                @tagName(self.peek().type),
            });
        }

        // Try parsing array literal first
        if (self.peek().type == .LEFT_BRACKET) {
            initializer = try expression_parser.parseArrayLiteral(self, null, .NONE);
        } else if (self.peek().type == .INPUT) {
            // Handle input expression
            initializer = try Parser.input(self, null, .NONE);
        } else if (self.peek().type == .IDENTIFIER) {
            // Try struct initialization
            if (try Parser.parseStructInit(self)) |struct_init| {
                initializer = struct_init;
            } else {
                // If not a struct init, try regular expression
                initializer = try expression_parser.parseExpression(self);
            }
        } else {
            initializer = try expression_parser.parseExpression(self);
        }
        if (initializer == null) {
            var reporter = Reporter.init();
            const location = Reporter.Location{
                .file = self.current_file,
                .line = self.peek().line,
                .column = self.peek().column,
            };
            reporter.reportCompileError(location, "Expected expression", .{});
            return error.ExpectedExpression;
        }
    }

    if (self.peek().type != .SEMICOLON) {
        if (self.debug_enabled) {
            std.debug.print("Expected semicolon, but found: {s}\n", .{@tagName(self.peek().type)});
        }
        var reporter = Reporter.init();
        const location = Reporter.Location{
            .file = self.current_file,
            .line = self.peek().line,
            .column = self.peek().column,
        };
        reporter.reportCompileError(location, "Expected semicolon", .{});
        return error.ExpectedSemicolon;
    }
    self.advance();

    if (type_info.base == .Auto and initializer == null) {
        var reporter = Reporter.init();
        const location = Reporter.Location{
            .file = self.current_file,
            .line = self.peek().line,
            .column = self.peek().column,
        };
        reporter.reportCompileError(location, "Auto type must have an initializer", .{});
        return error.ExpectedExpression;
    }

    if (type_info.base == .Auto and initializer != null) {
        // infer type from initializer
        const inferred_type = try expression_parser.inferType(initializer.?);
        type_info = inferred_type;
    }

    return ast.Stmt{ .VarDecl = .{
        .name = name,
        .type_info = type_info,
        .initializer = initializer,
        .is_public = is_public,
    } };
}

pub fn enumDeclPrefix(self: *Parser, _: ?*ast.Expr, _: Precedence) ErrorList!?*ast.Expr {
    const enum_stmt = try declaration_parser.parseEnumDecl(self);
    const expr = try self.allocator.create(ast.Expr);

    // Create a mutable copy of the variants array
    const variants = try self.allocator.alloc(token.Token, enum_stmt.EnumDecl.variants.len);
    @memcpy(variants, enum_stmt.EnumDecl.variants);

    expr.* = .{ .EnumDecl = .{
        .name = enum_stmt.EnumDecl.name,
        .variants = variants,
    } };
    return expr;
}
