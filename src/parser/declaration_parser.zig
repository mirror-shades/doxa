const std = @import("std");
const Parser = @import("./parser_types.zig").Parser;
const ast = @import("../ast/ast.zig");
const token = @import("../types/token.zig");
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

    // Consume 'enum' keyword
    self.advance();

    // Parse enum name
    if (self.peek().type != .IDENTIFIER) {
        return error.ExpectedIdentifier;
    }
    const name = self.peek();

    // Register the enum type name before advancing
    try self.declared_types.put(name.lexeme, {});

    self.advance();

    // Expect opening brace
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
        const variant_token = self.peek();
        try variants.append(variant_token);

        // Register each enum variant in declared_types for type resolution
        try self.declared_types.put(variant_token.lexeme, {});

        self.advance();

        if (self.peek().type == .COMMA) {
            self.advance();
        }
    }
    self.advance(); // consume right brace

    // Create source span from name token to closing brace
    const span = ast.SourceSpan{
        .start = .{
            .file = name.file,
            .line = name.line,
            .column = name.column,
        },
        .end = .{
            .file = self.previous().file,
            .line = self.previous().line,
            .column = self.previous().column + self.previous().lexeme.len,
        },
    };

    return ast.Stmt{
        .base = .{
            .id = ast.generateNodeId(),
            .span = span,
        },
        .data = .{
            .EnumDecl = .{
                .name = name,
                .variants = try variants.toOwnedSlice(),
                .is_public = is_public,
            },
        },
    };
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

    // Create source span from name token to closing brace
    const span = ast.SourceSpan{
        .start = .{
            .file = name.file,
            .line = name.line,
            .column = name.column,
        },
        .end = .{
            .file = self.previous().file,
            .line = self.previous().line,
            .column = self.previous().column + self.previous().lexeme.len,
        },
    };

    const struct_expr = try self.allocator.create(ast.Expr);
    struct_expr.* = .{
        .base = .{
            .id = ast.generateNodeId(),
            .span = span,
        },
        .data = .{
            .StructDecl = ast.StructDecl{
                .name = name,
                .fields = try fields.toOwnedSlice(),
                .is_public = is_public,
            },
        },
    };

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

    // Expect function keyword (both 'fn' and 'function' map to FUNCTION token)
    if (self.peek().type != .FUNCTION) {
        return error.ExpectedFunction;
    }
    self.advance(); // consume function keyword

    // Parse function name
    if (self.peek().type != .IDENTIFIER) {
        return error.ExpectedIdentifier;
    }
    const function_name = self.peek();
    self.advance(); // consume function name

    // If this is an entry point, store the function name
    if (is_entry) {
        self.entry_point_name = function_name.lexeme;
    }

    // Parse parameter list
    if (self.peek().type != .LEFT_PAREN) {
        return error.ExpectedLeftParen;
    }
    self.advance(); // consume '('

    // Parse parameters
    var params = std.ArrayList(ast.FunctionParam).init(self.allocator);
    errdefer params.deinit();

    if (self.peek().type != .RIGHT_PAREN) {
        while (true) {
            if (self.peek().type != .IDENTIFIER) {
                return error.ExpectedIdentifier;
            }
            const param_name = self.peek();
            self.advance(); // consume parameter name

            // Parse type annotation
            if (self.peek().type != .TYPE_SYMBOL) {
                return error.ExpectedTypeAnnotation;
            }
            self.advance(); // consume ::

            const type_expr = try expression_parser.parseTypeExpr(self) orelse return error.ExpectedType;

            // Parse default value if present
            var default_value: ?*ast.Expr = null;
            if (self.peek().type == .ASSIGN) {
                self.advance(); // consume 'is'
                default_value = try expression_parser.parseExpression(self);
            }

            try params.append(.{
                .name = param_name,
                .type_expr = type_expr,
                .default_value = default_value,
            });

            if (self.peek().type == .RIGHT_PAREN) break;
            if (self.peek().type != .COMMA) {
                return error.ExpectedCommaOrParen;
            }
            self.advance(); // consume comma
        }
    }
    self.advance(); // consume ')'

    // Parse return type if present
    var return_type = ast.TypeInfo{ .base = .Nothing }; // Default to Nothing type
    if (self.peek().type == .RETURNS) {
        self.advance(); // consume 'returns'
        if (self.peek().type != .LEFT_PAREN) {
            return error.ExpectedLeftParen;
        }
        self.advance(); // consume '('

        const type_expr = try expression_parser.parseTypeExpr(self) orelse return error.ExpectedType;
        const type_info_ptr = try ast.typeInfoFromExpr(self.allocator, type_expr);
        return_type = type_info_ptr.*;
        self.allocator.destroy(type_info_ptr); // Free the pointer since we copied the struct

        if (self.peek().type != .RIGHT_PAREN) {
            return error.ExpectedRightParen;
        }
        self.advance(); // consume ')'
    }

    // Parse function body
    if (self.peek().type != .LEFT_BRACE) {
        return error.ExpectedLeftBrace;
    }
    self.advance(); // consume '{'

    var body_statements = std.ArrayList(ast.Stmt).init(self.allocator);
    errdefer {
        for (body_statements.items) |*stmt| {
            stmt.deinit(self.allocator);
        }
        body_statements.deinit();
    }

    while (self.peek().type != .RIGHT_BRACE and self.peek().type != .EOF) {
        const stmt = try statement_parser.parseStatement(self);
        try body_statements.append(stmt);
    }

    if (self.peek().type != .RIGHT_BRACE) {
        return error.ExpectedRightBrace;
    }
    self.advance(); // consume '}'

    return ast.Stmt{
        .base = .{
            .id = ast.generateNodeId(),
            .span = ast.SourceSpan.fromToken(function_name),
        },
        .data = .{
            .FunctionDecl = .{
                .name = function_name,
                .params = try params.toOwnedSlice(),
                .return_type_info = return_type,
                .body = try body_statements.toOwnedSlice(),
                .is_entry = is_entry,
                .is_public = is_public,
            },
        },
    };
}

pub fn parseVarDecl(self: *Parser) ErrorList!ast.Stmt {
    const is_public = (self.peek().type == .PUBLIC) or
        (self.previous().type == .PUBLIC and
            (self.peek().type == .VAR or self.peek().type == .CONST));
    var is_const = false;
    // Check if this is a var or const declaration
    if (self.peek().type == .VAR or self.peek().type == .CONST) {
        is_const = self.peek().type == .CONST;
        self.advance(); // consume 'var' or 'const'
    } else {
        return error.ExpectedVarOrConst;
    }

    // Initialize with default Nothing type
    var type_info = ast.TypeInfo{ .base = .Nothing, .is_mutable = !is_const };

    // Check for explicit type annotation
    if (self.peek().type == .TYPE_SYMBOL) {
        self.advance(); // consume ::
        const type_expr = try expression_parser.parseTypeExpr(self) orelse return error.ExpectedType;
        const type_info_ptr = try ast.typeInfoFromExpr(self.allocator, type_expr);
        type_info = type_info_ptr.*;
        self.allocator.destroy(type_info_ptr); // Free the pointer since we copied the struct
        type_info.is_mutable = !is_const; // Preserve mutability
    }

    // Special handling for array type declarations
    if (self.peek().type == .ARRAY_TYPE) {
        type_info.base = .Array;
        self.advance(); // consume 'array'

        // Create implicit name for array
        const name = token.Token.initWithFile(.IDENTIFIER, "array", .{ .nothing = {} }, 0, 0, self.current_file);

        var initializer: ?*ast.Expr = null;
        if (self.peek().type == .ASSIGN) {
            self.advance();

            initializer = try expression_parser.parseExpression(self);
            if (initializer == null) {
                const location = Reporter.Location{
                    .file = self.current_file,
                    .line = self.peek().line,
                    .column = self.peek().column,
                };
                self.reporter.reportCompileError(location, "Expected expression", .{});
                return error.ExpectedExpression;
            }
        }

        if (self.peek().type != .SEMICOLON) {
            return error.ExpectedSemicolon;
        }
        self.advance();

        return ast.Stmt{
            .base = .{
                .id = ast.generateNodeId(),
                .span = ast.SourceSpan.fromToken(name),
            },
            .data = .{
                .VarDecl = .{
                    .name = name,
                    .type_info = type_info,
                    .initializer = initializer,
                    .is_public = is_public,
                },
            },
        };
    }

    // Original variable declaration parsing logic
    const name = if (self.peek().type != .IDENTIFIER) {
        return error.ExpectedIdentifier;
    } else blk: {
        const n = self.peek();
        self.advance();
        break :blk n;
    };

    if (self.peek().type == .WHERE) {
        const location = Reporter.Location{
            .file = self.current_file,
            .line = self.peek().line,
            .column = self.peek().column,
        };
        self.reporter.reportCompileError(location, "Type declaration uses ::", .{});
        return error.ExpectedTypeAnnotation;
    }

    // Handle type annotation (only if not already parsed above)
    if (self.peek().type == .TYPE_SYMBOL and type_info.base == .Nothing) {
        self.advance(); // consume ::

        // Check if this is a union type using pipe syntax (int | float | ErrorEnum)
        if (self.peek().type == .PIPE or self.peekAhead(1).type == .PIPE) {
            var types = std.ArrayList(*ast.TypeInfo).init(self.allocator);
            defer types.deinit();

            // Parse the first type
            const first_type_expr = try expression_parser.parseTypeExpr(self) orelse return error.ExpectedType;
            const first_type_info = try ast.typeInfoFromExpr(self.allocator, first_type_expr);
            try types.append(first_type_info);

            // Parse additional types separated by pipes
            while (self.peek().type == .PIPE) {
                self.advance(); // consume |
                const type_expr = try expression_parser.parseTypeExpr(self) orelse return error.ExpectedType;
                const next_type_info = try ast.typeInfoFromExpr(self.allocator, type_expr);
                try types.append(next_type_info);
            }

            // Create union type
            const union_type = try self.allocator.create(ast.UnionType);
            union_type.* = .{
                .types = try types.toOwnedSlice(),
                .current_type_index = 0, // Default to first type
            };

            // The memory manager will handle this type_info through ValueStorage
            type_info = .{
                .base = .Union,
                .union_type = union_type,
                .is_mutable = !is_const,
            };
        } else {
            // Regular type annotation
            const type_expr = try expression_parser.parseTypeExpr(self) orelse return error.ExpectedType;

            // Use the typeInfoFromExpr function to properly convert TypeExpr to TypeInfo
            const type_info_ptr = try ast.typeInfoFromExpr(self.allocator, type_expr);
            type_info = type_info_ptr.*;
            self.allocator.destroy(type_info_ptr); // Free the pointer since we copied the struct

            // Preserve mutability from our original type_info
            type_info.is_mutable = !is_const;
        }
    }

    if (self.debug_enabled) {
        std.debug.print("After parsing type annotation. Current token: {s} ('{s}') at position {}\n", .{ @tagName(self.peek().type), self.peek().lexeme, self.current });
    }

    var initializer: ?*ast.Expr = null;

    if (self.debug_enabled) {
        std.debug.print("Before checking for initializer. Current token: {s} ('{s}')\n", .{ @tagName(self.peek().type), self.peek().lexeme });
    }

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

        // Parse regular initializer expression
        // Try parsing array literal first
        if (self.peek().type == .LEFT_BRACKET) {
            if (self.debug_enabled) {
                std.debug.print("About to parse array literal. Current token: {s} ('{s}')\n", .{ @tagName(self.peek().type), self.peek().lexeme });
            }
            initializer = try expression_parser.parseArrayLiteral(self, null, .NONE);
            if (self.debug_enabled) {
                std.debug.print("After parsing array literal. Current token: {s} ('{s}')\n", .{ @tagName(self.peek().type), self.peek().lexeme });
            }
        } else if (self.peek().type == .INPUT) {
            // Handle input expression
            initializer = try Parser.input(self, null, .NONE);
        } else if (self.peek().type == .IDENTIFIER) {
            // Try struct initialization
            if (try Parser.parseStructInit(self)) |struct_init| {
                initializer = struct_init;
            } else {
                // If not a struct init, try regular expression
                if (self.debug_enabled) {
                    std.debug.print("\n[DEBUG] About to parse initializer expression at position {}, token: {s} ('{s}')\n", .{ self.current, @tagName(self.peek().type), self.peek().lexeme });
                    if (self.current + 1 < self.tokens.len) {
                        std.debug.print("[DEBUG] Next token: {s} ('{s}')\n", .{ @tagName(self.tokens[self.current + 1].type), self.tokens[self.current + 1].lexeme });
                    }
                }
                initializer = try expression_parser.parseExpression(self);
                if (self.debug_enabled) {
                    std.debug.print("[DEBUG] Finished parsing initializer expression. Next token: {s} ('{s}')\n", .{ @tagName(self.peek().type), self.peek().lexeme });
                }
            }
        } else {
            if (self.debug_enabled) {
                std.debug.print("\n[DEBUG] About to parse initializer expression at position {}, token: {s}\n", .{ self.current, @tagName(self.peek().type) });
            }
            initializer = try expression_parser.parseExpression(self);
            if (self.debug_enabled) {
                std.debug.print("[DEBUG] Finished parsing initializer expression. Next token: {s}\n", .{@tagName(self.peek().type)});
            }
        }

        if (initializer == null) {
            return error.ExpectedExpression;
        }
    }

    // Validate that const declarations must have initializers
    if (is_const and initializer == null) {
        const location = Reporter.Location{
            .file = self.current_file,
            .line = name.line,
            .column = name.column,
        };
        self.reporter.reportCompileError(location, "Const declarations must have initializers", .{});
        return error.ConstMustHaveInitializer;
    }

    // Make semicolons optional - consume if present, but don't require them
    if (self.peek().type == .SEMICOLON) {
        self.advance(); // Consume the semicolon if present
    } else {
        if (self.debug_enabled) {
            std.debug.print("No semicolon found, continuing. Next token: {s}\n", .{@tagName(self.peek().type)});
        }
        // Continue without semicolon
    }

    // CRITICAL: Reject variable declarations with no type annotation and no initializer
    if (type_info.base == .Nothing and initializer == null) {
        const location = Reporter.Location{
            .file = self.current_file,
            .line = name.line,
            .column = name.column,
        };
        self.reporter.reportCompileError(location, "Variable declaration requires either type annotation (::) or initializer", .{});
        return error.ExpectedTypeAnnotationOrInitializer;
    }

    if (self.debug_enabled) {
        std.debug.print("Finished parsing variable declaration. Current token: {s} ('{s}')\n", .{ @tagName(self.peek().type), self.peek().lexeme });
    }

    // Handle type inference and union initialization
    if (type_info.base == .Union and initializer != null) {
        // For unions, we need to determine which type the initializer matches
        const union_type = type_info.union_type orelse return error.TypeMismatch;
        const inferred_type = try expression_parser.inferType(initializer.?);

        // Find which type in the union matches the initializer
        var found_match = false;
        for (union_type.types, 0..) |union_member_type, i| {
            if (union_member_type.base == inferred_type.base) {
                union_type.setCurrentType(@intCast(i));
                found_match = true;
                break;
            }
        }

        if (!found_match) {
            // TODO: Add better error reporting for union type mismatch
            return error.TypeMismatch;
        }
    } else if (type_info.base == .Nothing and initializer != null) {
        // infer type from initializer for non-union types
        const inferred_type = try expression_parser.inferType(initializer.?);
        // Preserve mutability from the original declaration (var vs const)
        const original_mutability = type_info.is_mutable;
        type_info = inferred_type;
        type_info.is_mutable = original_mutability;
    }

    return ast.Stmt{
        .base = .{
            .id = ast.generateNodeId(),
            .span = ast.SourceSpan.fromToken(name),
        },
        .data = .{
            .VarDecl = .{
                .name = name,
                .type_info = type_info,
                .initializer = initializer,
                .is_public = is_public,
            },
        },
    };
}

pub fn enumDeclPrefix(self: *Parser, _: ?*ast.Expr, _: Precedence) ErrorList!?*ast.Expr {
    const enum_stmt = try declaration_parser.parseEnumDecl(self);
    const expr = try self.allocator.create(ast.Expr);

    // Create a mutable copy of the variants array
    const variants = try self.allocator.alloc(token.Token, enum_stmt.data.EnumDecl.variants.len);
    @memcpy(variants, enum_stmt.data.EnumDecl.variants);

    expr.* = .{
        .base = .{
            .id = ast.generateNodeId(),
            .span = ast.SourceSpan.fromToken(enum_stmt.data.EnumDecl.name),
        },
        .data = .{ .EnumDecl = .{
            .name = enum_stmt.data.EnumDecl.name,
            .variants = variants,
            .is_public = enum_stmt.data.EnumDecl.is_public,
        } },
    };
    return expr;
}
