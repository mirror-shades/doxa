const std = @import("std");
const Parser = @import("parser_types.zig").Parser;
const ast = @import("../ast.zig");
const token = @import("../token.zig");
const ErrorList = @import("../reporting.zig").ErrorList;
const expression_parser = @import("expression_parser.zig");
const Precedence = @import("./precedence.zig").Precedence;
const statement_parser = @import("./statement_parser.zig");
const precedence = @import("./precedence.zig");
const declaration_parser = @import("./declaration_parser.zig");

pub fn parseEnumDecl(self: *Parser) ErrorList!ast.Stmt {
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
    } };
}

pub fn parseStructDecl(self: *Parser, _: ?*ast.Expr, _: Precedence) ErrorList!?*ast.Expr {
    // Consume 'struct' keyword
    self.advance();

    // Parse struct name
    if (self.peek().type != .IDENTIFIER) {
        return error.ExpectedIdentifier;
    }
    const name = self.peek();
    self.advance();

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
        if (self.peek().type != .WHERE_SYMBOL) {
            return error.ExpectedColon;
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
    } };

    return struct_expr;
}

pub fn parseFunctionDecl(self: *Parser) ErrorList!ast.Stmt {
    var is_entry = false;

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
    if (self.peek().type != .FN_KEYWORD and self.peek().type != .FUNCTION_KEYWORD) {
        return error.ExpectedFunction;
    }
    self.advance(); // consume fn/function keyword

    if (self.peek().type != .IDENTIFIER) {
        return error.ExpectedIdentifier;
    }
    const name = self.peek();

    // If this is an entry point, verify it's named 'main'
    if (is_entry and !std.mem.eql(u8, name.lexeme, "main")) {
        return error.EntryPointMustBeMain;
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

            // Parse return type
            const type_name = self.peek();
            self.advance();
            return_type.base = switch (type_name.type) {
                .INT_TYPE => .Int,
                .FLOAT_TYPE => .Float,
                .STRING_TYPE => .String,
                .BOOLEAN_TYPE => .Boolean,
                .TETRA_TYPE => .Tetra,
                else => return error.InvalidType,
            };

            if (self.peek().type != .RIGHT_PAREN) {
                return error.ExpectedRightParen;
            }
            self.advance();
        } else {
            return error.ExpectedLeftParen; // In Safe Mode, must use returns(type) syntax
        }
    }

    if (self.mode == .Safe) {
        // In safe mode:
        // 1. All parameters must have types
        for (params.items) |param| {
            if (param.type_expr == null) {
                return error.MissingParameterType;
            }
        }

        // 2. If function has any return statements with values, must use returns(type)
        if ((try Parser.hasReturnWithValue(self)) and !has_return_type) {
            return error.MissingReturnType;
        }
    }

    // Parse function body
    if (self.peek().type != .LEFT_BRACE) {
        return error.ExpectedLeftBrace;
    }

    const body = try statement_parser.parseBlockStmt(self);

    return ast.Stmt{ .Function = .{
        .name = name,
        .params = try params.toOwnedSlice(),
        .return_type_info = return_type,
        .body = body,
        .is_entry = is_entry,
    } };
}

pub fn parseVarDecl(self: *Parser) ErrorList!ast.Stmt {
    const is_const = self.peek().type == .CONST;
    self.advance(); // consume 'var' or 'const'

    if (self.debug_enabled) {
        std.debug.print("\nParsing var declaration\n", .{});
        std.debug.print("Current token: {s}\n", .{@tagName(self.peek().type)});
    }

    var type_info: ast.TypeInfo = .{
        .base = .Dynamic,
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
        if (self.peek().type == .ASSIGN_SYMBOL or self.peek().type == .ASSIGN_KEYWORD) {
            self.advance();
            if (self.debug_enabled) {
                std.debug.print("\nParsing array initializer at position {}, token: {s}\n", .{
                    self.current,
                    @tagName(self.peek().type),
                });
            }

            initializer = try expression_parser.parseExpression(self);
            if (initializer == null) return error.ExpectedExpression;
        }

        if (self.peek().type != .SEMICOLON) {
            if (self.debug_enabled) {
                std.debug.print("Expected semicolon, but found: {s}\n", .{@tagName(self.peek().type)});
            }
            return error.ExpectedSemicolon;
        }
        self.advance();

        return ast.Stmt{ .VarDecl = .{
            .name = name,
            .type_info = type_info,
            .initializer = initializer,
        } };
    }

    // Original variable declaration parsing logic
    const name = if (self.peek().type != .IDENTIFIER) {
        return error.ExpectedIdentifier;
    } else blk: {
        const n = self.peek();
        self.advance();
        break :blk n;
    };

    if (self.debug_enabled) {
        std.debug.print("After name, current token: {s}\n", .{@tagName(self.peek().type)});
    }

    // Handle type annotation
    if (self.peek().type == .TYPE_SYMBOL) {
        self.advance(); // consume ::
        type_info.is_dynamic = false; // Explicitly typed variables are not dynamic

        type_info.base = switch (self.peek().type) {
            .INT_TYPE => .Int,
            .FLOAT_TYPE => .Float,
            .STRING_TYPE => .String,
            .BOOLEAN_TYPE => .Boolean,
            .TETRA_TYPE => .Tetra,
            .IDENTIFIER => blk: {
                const type_name = self.peek().lexeme;
                // Check built-in types first
                if (std.mem.eql(u8, type_name, "bool")) {
                    break :blk .Boolean;
                } else if (std.mem.eql(u8, type_name, "int")) {
                    break :blk .Int;
                } else if (std.mem.eql(u8, type_name, "float")) {
                    break :blk .Float;
                } else if (std.mem.eql(u8, type_name, "string")) {
                    break :blk .String;
                } else if (self.declared_types.contains(type_name)) {
                    // It's a valid custom type
                    type_info.custom_type = type_name;
                    break :blk .Custom;
                } else {
                    return error.UndefinedType;
                }
            },
            else => return error.ExpectedType,
        };
        self.advance(); // consume type identifier

        // Check for array type with [] syntax
        while (self.peek().type == .LEFT_BRACKET) {
            self.advance(); // consume '['
            if (self.peek().type != .RIGHT_BRACKET) {
                return error.ExpectedRightBracket;
            }
            self.advance(); // consume ']'

            // Wrap the current type in an array type
            type_info = ast.TypeInfo{
                .base = .Array,
                .element_type = type_info.base,
                .is_dynamic = false,
                .is_mutable = !is_const,
            };
        }
    } else if (self.mode == .Safe) {
        return error.MissingTypeAnnotation;
    }

    var initializer: ?*ast.Expr = null;
    if (self.peek().type == .ASSIGN_SYMBOL or self.peek().type == .ASSIGN_KEYWORD) {
        self.advance();
        if (self.debug_enabled) {
            std.debug.print("\nParsing var initializer at position {}, token: {s}\n", .{
                self.current,
                @tagName(self.peek().type),
            });
        }

        // Try parsing struct initialization first
        if (self.peek().type == .IDENTIFIER) {
            // First try struct initialization
            if (try Parser.parseStructInit(self)) |struct_init| {
                initializer = struct_init;
            } else {
                // If not a struct init, try regular expression
                initializer = try expression_parser.parseExpression(self);
            }
        } else {
            initializer = try expression_parser.parseExpression(self);
        }
        if (initializer == null) return error.ExpectedExpression;
    }

    if (self.peek().type != .SEMICOLON) {
        if (self.debug_enabled) {
            std.debug.print("Expected semicolon, but found: {s}\n", .{@tagName(self.peek().type)});
        }
        return error.ExpectedSemicolon;
    }
    self.advance();

    return ast.Stmt{ .VarDecl = .{
        .name = name,
        .type_info = type_info,
        .initializer = initializer,
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
