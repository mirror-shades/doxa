const std = @import("std");
const Parser = @import("./parser_types.zig").Parser;
const ast = @import("../ast/ast.zig");
const token = @import("../types/token.zig");
const Reporting = @import("../utils/reporting.zig");
const Reporter = Reporting.Reporter;
const Location = Reporting.Location;
const Errors = @import("../utils/errors.zig");
const ErrorList = Errors.ErrorList;
const expression_parser = @import("./expression_parser.zig");
const Precedence = @import("./precedence.zig").Precedence;
const statement_parser = @import("./statement_parser.zig");
const precedence = @import("./precedence.zig");
const declaration_parser = @import("./declaration_parser.zig");
const ErrorCode = @import("../utils/errors.zig").ErrorCode;

pub fn parseEnumDecl(self: *Parser) ErrorList!ast.Stmt {
    var is_public = false;
    if (self.peek().type == .PUBLIC) {
        is_public = true;
        self.advance();
    }

    self.advance();

    if (self.peek().type != .IDENTIFIER) {
        return error.ExpectedIdentifier;
    }
    const name = self.peek();

    try self.declared_types.put(name.lexeme, {});

    self.advance();
    if (self.peek().type != .LEFT_BRACE) {
        return error.ExpectedLeftBrace;
    }
    self.advance();

    var variants = std.array_list.Managed(token.Token).init(self.allocator);
    errdefer variants.deinit();

    while (self.peek().type != .RIGHT_BRACE) {
        if (self.peek().type == .NEWLINE) {
            self.advance();
            continue;
        }
        if (self.peek().type != .IDENTIFIER) {
            return error.ExpectedIdentifier;
        }
        const variant_token = self.peek();
        try variants.append(variant_token);

        try self.declared_types.put(variant_token.lexeme, {});

        self.advance();

        if (self.peek().type == .COMMA) {
            self.advance();
        }
        while (self.peek().type == .NEWLINE) {
            self.advance();
        }
    }
    self.advance();

    const span = ast.SourceSpan{
        .location = .{
            .file = name.file,
            .range = .{
                .start_line = @intCast(name.line),
                .start_col = name.column,
                .end_line = @intCast(self.previous().line),
                .end_col = self.previous().column + self.previous().lexeme.len,
            },
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
    var is_public = false;
    if (self.peek().type == .PUBLIC) {
        is_public = true;
        self.advance();
    }

    self.advance();

    if (self.peek().type != .IDENTIFIER) {
        return error.ExpectedIdentifier;
    }
    const name = self.peek();

    try self.declared_types.put(name.lexeme, {});

    self.advance();

    if (self.peek().type != .LEFT_BRACE) {
        return error.ExpectedLeftBrace;
    }
    self.advance();

    var fields = std.array_list.Managed(*ast.StructField).init(self.allocator);
    var methods = std.array_list.Managed(*ast.StructMethod).init(self.allocator);
    errdefer {
        for (fields.items) |field| {
            field.deinit(self.allocator);
            self.allocator.destroy(field);
        }
        fields.deinit();
        for (methods.items) |method| {
            method.deinit(self.allocator);
            self.allocator.destroy(method);
        }
        methods.deinit();
    }

    while (self.peek().type != .RIGHT_BRACE) {
        while (self.peek().type == .NEWLINE) self.advance();

        var is_public_method = false;
        if (self.peek().type == .PUBLIC) {
            is_public_method = true;
            self.advance();
        }

        if (self.peek().type == .METHOD or self.peek().type == .FUNCTION) {
            const is_function = self.peek().type == .FUNCTION;
            self.advance();

            if (self.peek().type != .IDENTIFIER) {
                return error.ExpectedIdentifier;
            }
            const method_name = self.peek();
            self.advance();

            const method_is_static: bool = is_function;

            var params = std.array_list.Managed(ast.FunctionParam).init(self.allocator);
            errdefer {
                for (params.items) |*param| {
                    param.deinit(self.allocator);
                }
                params.deinit();
            }

            if (self.peek().type == .LEFT_PAREN) {
                self.advance();

                while (self.peek().type != .RIGHT_PAREN) {
                    while (self.peek().type == .NEWLINE) self.advance();

                    if (self.peek().type != .IDENTIFIER) {
                        return error.ExpectedIdentifier;
                    }
                    const param_name = self.peek();
                    self.advance();

                    var param_type: ?*ast.TypeExpr = null;
                    if (self.peek().type == .TYPE_SYMBOL) {
                        self.advance();
                        param_type = try expression_parser.parseTypeExpr(self) orelse return error.ExpectedType;
                    }

                    const param = ast.FunctionParam{
                        .name = param_name,
                        .type_expr = param_type,
                        .default_value = null,
                        .is_alias = false,
                    };
                    try params.append(param);

                    if (self.peek().type == .COMMA) {
                        self.advance();
                    }
                    while (self.peek().type == .NEWLINE) self.advance();
                }
                self.advance();
            }

            var return_type_info = ast.TypeInfo{ .base = .Nothing };
            if (self.peek().type == .RETURNS) {
                self.advance();
                const return_type_expr = try expression_parser.parseTypeExpr(self) orelse return error.ExpectedType;
                const return_type_ptr = try ast.typeInfoFromExpr(self.allocator, return_type_expr);
                return_type_info = return_type_ptr.*;
                self.allocator.destroy(return_type_ptr);
            }

            while (self.peek().type == .NEWLINE) self.advance();

            if (self.peek().type != .LEFT_BRACE) {
                return error.ExpectedLeftBrace;
            }
            self.advance();

            var body = std.array_list.Managed(ast.Stmt).init(self.allocator);
            errdefer {
                for (body.items) |*stmt| {
                    stmt.deinit(self.allocator);
                }
                body.deinit();
            }

            while (self.peek().type != .RIGHT_BRACE) {
                while (self.peek().type == .NEWLINE) self.advance();
                const stmt = try statement_parser.parseStatement(self);
                try body.append(stmt);
            }
            self.advance();

            const method = try self.allocator.create(ast.StructMethod);
            method.* = .{
                .name = method_name,
                .params = try params.toOwnedSlice(),
                .return_type_info = return_type_info,
                .body = try body.toOwnedSlice(),
                .is_public = is_public_method,
                .is_static = method_is_static,
            };
            try methods.append(method);
        } else {
            if (self.peek().type != .IDENTIFIER) {
                return error.ExpectedIdentifier;
            }
            const field_name = self.peek();
            self.advance();

            if (self.peek().type != .TYPE_SYMBOL) {
                return error.ExpectedTypeAnnotation;
            }
            self.advance();

            const type_expr = try expression_parser.parseTypeExpr(self) orelse return error.ExpectedType;

            const field = try self.allocator.create(ast.StructField);
            field.* = .{
                .name = field_name,
                .type_expr = type_expr,
                .is_public = is_public_method,
            };
            try fields.append(field);
        }

        if (self.peek().type == .COMMA) {
            self.advance();
        }
        while (self.peek().type == .NEWLINE) self.advance();
    }

    self.advance();

    const span = ast.SourceSpan{
        .location = .{
            .file = name.file,
            .range = .{
                .start_line = @intCast(name.line),
                .start_col = name.column,
                .end_line = @intCast(self.previous().line),
                .end_col = self.previous().column + self.previous().lexeme.len,
            },
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
                .methods = try methods.toOwnedSlice(),
                .is_public = is_public,
            },
        },
    };

    return struct_expr;
}

pub fn parseFunctionDecl(self: *Parser) ErrorList!ast.Stmt {
    var is_entry = false;
    var is_public = false;

    if (self.peek().type == .PUBLIC) {
        is_public = true;
        self.advance();
    }

    if (self.peek().type == .ENTRY) {
        is_entry = true;

        if (self.has_entry_point) {
            return error.MultipleEntryPoints;
        }

        self.has_entry_point = true;
        self.entry_point_location = self.peek();
        self.advance();
    }

    if (self.peek().type != .FUNCTION) {
        return error.ExpectedFunction;
    }
    self.advance();

    if (self.peek().type != .IDENTIFIER) {
        return error.ExpectedIdentifier;
    }
    const function_name = self.peek();
    self.advance();

    if (is_entry) {
        self.entry_point_name = function_name.lexeme;
    }

    if (self.peek().type != .LEFT_PAREN) {
        return error.ExpectedLeftParen;
    }
    self.advance();

    var params = std.array_list.Managed(ast.FunctionParam).init(self.allocator);
    errdefer params.deinit();

    if (self.peek().type != .RIGHT_PAREN) {
        while (true) {
            var is_alias = false;
            if (self.peek().type == .CARET) {
                self.advance();
                is_alias = true;
            }
            if (self.peek().type != .IDENTIFIER) {
                return error.ExpectedIdentifier;
            }
            const param_name = self.peek();
            self.advance();

            if (self.peek().type != .TYPE_SYMBOL) {
                return error.ExpectedTypeAnnotation;
            }
            self.advance();

            const type_expr = try expression_parser.parseTypeExpr(self) orelse return error.ExpectedType;

            var default_value: ?*ast.Expr = null;
            if (self.peek().type == .ASSIGN) {
                self.advance();
                default_value = try expression_parser.parseExpression(self);
            }

            try params.append(.{
                .name = param_name,
                .type_expr = type_expr,
                .default_value = default_value,
                .is_alias = is_alias,
            });

            if (self.peek().type == .RIGHT_PAREN) break;
            if (self.peek().type != .COMMA) {
                return error.ExpectedCommaOrParen;
            }
            self.advance();
        }
    }
    self.advance();

    var return_type = ast.TypeInfo{ .base = .Nothing };
    if (self.peek().type == .RETURNS) {
        self.advance();

        const type_expr = try expression_parser.parseTypeExpr(self) orelse return error.ExpectedType;
        const type_info_ptr = try ast.typeInfoFromExpr(self.allocator, type_expr);
        return_type = type_info_ptr.*;
        self.allocator.destroy(type_info_ptr);
    }

    if (self.peek().type != .LEFT_BRACE) {
        return error.ExpectedLeftBrace;
    }
    self.advance();

    var body_statements = std.array_list.Managed(ast.Stmt).init(self.allocator);
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
    self.advance();

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
    if (self.peek().type == .VAR or self.peek().type == .CONST) {
        is_const = self.peek().type == .CONST;
        self.advance();
    } else {
        return error.ExpectedVarOrConst;
    }

    var type_info = ast.TypeInfo{ .base = .Nothing, .is_mutable = !is_const };

    if (self.peek().type == .TYPE_SYMBOL) {
        self.advance();
        const type_expr = try expression_parser.parseTypeExpr(self) orelse return error.ExpectedType;
        const type_info_ptr = try ast.typeInfoFromExpr(self.allocator, type_expr);
        type_info = type_info_ptr.*;
        self.allocator.destroy(type_info_ptr);
        type_info.is_mutable = !is_const;
    }

    if (self.peek().type == .ARRAY_TYPE) {
        type_info.base = .Array;
        self.advance();

        const name = token.Token.initWithFile(.IDENTIFIER, "array", .{ .nothing = {} }, 0, 0, self.current_file);

        var initializer: ?*ast.Expr = null;
        if (self.peek().type == .ASSIGN) {
            self.advance();

            initializer = try expression_parser.parseExpression(self);
            if (initializer == null) {
                const location = Reporting.Location{
                    .file = self.current_file,
                    .range = .{
                        .start_line = self.peek().line,
                        .start_col = self.peek().column,
                        .end_line = self.peek().line,
                        .end_col = self.peek().column,
                    },
                };
                self.reporter.reportCompileError(location, null, "Expected expression", .{});
                return error.ExpectedExpression;
            }
        }

        if (self.peek().type != .NEWLINE) {
            return error.ExpectedNewline;
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

    if (self.peek().type == .MAP_TYPE) {
        self.advance();

        if (self.peek().type != .IDENTIFIER) return error.ExpectedIdentifier;
        const map_name = self.peek();
        self.advance();

        if (self.peek().type == .TYPE_SYMBOL) {
            self.advance();
            _ = try expression_parser.parseTypeExpr(self) orelse return error.ExpectedType;
            if (self.peek().type == .RETURNS) {
                self.advance();
                _ = try expression_parser.parseTypeExpr(self) orelse return error.ExpectedType;
            }
        } else if (self.peek().type == .RETURNS) {
            self.advance();
            _ = try expression_parser.parseTypeExpr(self) orelse return error.ExpectedType;
        }

        if (self.peek().type != .LEFT_BRACE) return error.ExpectedLeftBrace;
        self.advance();
        const map_expr = try self.parseMap() orelse return error.ExpectedExpression;

        var inferred: ast.TypeInfo = .{ .base = .Nothing };
        inferred.is_mutable = !is_const;

        if (self.peek().type == .NEWLINE) self.advance();

        return ast.Stmt{
            .base = .{ .id = ast.generateNodeId(), .span = ast.SourceSpan.fromToken(map_name) },
            .data = .{ .VarDecl = .{ .name = map_name, .type_info = inferred, .initializer = map_expr, .is_public = is_public } },
        };
    }

    const name = if (self.peek().type != .IDENTIFIER) {
        return error.ExpectedIdentifier;
    } else blk: {
        const n = self.peek();
        self.advance();
        break :blk n;
    };

    if (self.peek().type == .WHERE) {
        const location = Location{
            .file = self.current_file,
            .range = .{
                .start_line = self.peek().line,
                .start_col = self.peek().column,
                .end_line = self.peek().line,
                .end_col = self.peek().column,
            },
        };
        self.reporter.reportCompileError(location, ErrorCode.EXPECTED_TYPE_ANNOTATION, "Expected type annotation", .{});
        return error.ExpectedTypeAnnotation;
    }

    if (self.peek().type == .TYPE_SYMBOL and type_info.base == .Nothing) {
        self.advance();

        const type_expr = try expression_parser.parseTypeExpr(self) orelse return error.ExpectedType;

        const type_info_ptr = try ast.typeInfoFromExpr(self.allocator, type_expr);
        type_info = type_info_ptr.*;
        self.allocator.destroy(type_info_ptr);

        type_info.is_mutable = !is_const;
    }

    var initializer: ?*ast.Expr = null;

    if (self.peek().type == .INPUT) {
        initializer = try Parser.input(self, null, .NONE);
        if (initializer == null) return error.ExpectedExpression;
    } else if (self.peek().type == .ASSIGN) {
        self.advance();

        if (self.peek().type == .INPUT) {
            initializer = try Parser.input(self, null, .NONE);
        } else if (self.peek().type == .IDENTIFIER) {
            if (try Parser.parseStructInit(self)) |struct_init| {
                initializer = struct_init;
            } else {
                initializer = try expression_parser.parseExpression(self);
            }
        } else {
            initializer = try expression_parser.parseExpression(self);
        }

        if (initializer == null) {
            return error.ExpectedExpression;
        }
    }

    if (is_const and initializer == null) {
        const location = Location{
            .file = self.current_file,
            .range = .{
                .start_line = name.line,
                .start_col = name.column,
                .end_line = name.line,
                .end_col = name.column,
            },
        };
        self.reporter.reportCompileError(location, ErrorCode.EXPECTED_TYPE_ANNOTATION, "Expected type annotation", .{});
        return error.ConstMustHaveInitializer;
    }

    if (self.peek().type == .NEWLINE) {
        self.advance();
    }

    if (type_info.base == .Nothing and initializer == null) {
        const location = Location{
            .file = self.current_file,
            .range = .{
                .start_line = name.line,
                .start_col = name.column,
                .end_line = name.line,
                .end_col = name.column,
            },
        };
        self.reporter.reportCompileError(
            location,
            ErrorCode.EXPECTED_TYPE_ANNOTATION,
            "Expected type annotation",
            .{},
        );
        return error.ExpectedTypeAnnotationOrInitializer;
    }

    if (type_info.base == .Union and initializer != null) {} else if (type_info.base == .Nothing and initializer != null) {
        const inferred_type = try expression_parser.inferType(initializer.?);
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
