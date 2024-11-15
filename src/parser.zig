const std = @import("std");
const Lexer = @import("lexer.zig").Lexer;
const TokenKind = @import("lexer.zig").TokenKind;
const Token = @import("lexer.zig").Token;
const Node = @import("ast.zig").Node;
const Type = @import("ast.zig").Type;
const ArrayList = std.ArrayList;

pub const Parser = struct {
    pub const Error = error{
        UnexpectedToken,
        MissingClosingParenthesis,
        ExpectedSemicolon,
        ExpectedEqual,
        ExpectedIdentifier,
        ExpectedType,
        EndOfFile,
        OutOfMemory,
        InvalidNumber,
        UnmatchedBrace,
        EmptyConstantDeclaration
    };

    lexer: *Lexer,
    current_token: Token,
    allocator: *std.mem.Allocator,

    pub fn init(lexer: *Lexer, allocator: *std.mem.Allocator) Parser {
        var parser = Parser{
            .lexer = lexer,
            .current_token = Token{ .kind = .EOF, .lexeme = "" },
            .allocator = allocator,
        };
        parser.advance();
        return parser;
    }

    fn advance(self: *Parser) void {
        self.current_token = self.lexer.nextToken();
    }

    fn peekToken(self: *Parser) Token {
        // Save the current lexer state
        const saved_current = self.lexer.current;

        // Get the next token
        const next_token = self.lexer.nextToken();

        // Restore the lexer state
        self.lexer.current = saved_current;

        return next_token;
    }

    fn match(self: *Parser, kind: TokenKind) bool {
        if (self.current_token.kind == kind) {
            self.advance();
            return true;
        }
        return false;
    }

    fn check(self: *Parser, kind: TokenKind) bool {
        return self.current_token.kind == kind;
    }

    pub fn parseProgram(self: *Parser) ![]*Node {
        var statements = ArrayList(*Node).init(self.allocator);

        while (self.current_token.kind != .EOF) {
            const stmt = try self.parseStatement();
            try statements.append(stmt);
        }

        return statements.toOwnedSlice();
    }

    pub fn parseStatement(self: *Parser) Error!*Node {
        var node: *Node = undefined;

        if (self.current_token.kind == .EOF) {
            // Nothing to parse; return null or an appropriate value
            return error.EndOfFile;
        }

        if (self.match(.LeftBrace)) {
            node = try self.parseBlock();
            return node;
        } else if (self.match(.Print)) {
            node = try self.parsePrintStatement();
        } else if (self.current_token.kind == .Var or self.current_token.kind == .Const) {
            node = try self.parseVariableDeclaration();
        } else if (self.current_token.kind == .Identifier and self.peekToken().kind == .Equal) {
            node = try self.parseAssignment();
        } else {
            node = try self.parseExpression();
        }

        // Expect semicolon after the statement
        if (self.current_token.kind != .Semicolon) {
            return Error.ExpectedSemicolon;
        }
        self.advance(); // Consume ';'

        return node;
    }

    fn parsePrintStatement(self: *Parser) !*Node {
        const expression = try self.parseExpression();

        const print_node = try self.allocator.create(Node);
        print_node.* = .{ .Print = .{ .expression = expression } };
        return print_node;
    }

    fn parseVariableDeclaration(self: *Parser) !*Node {
        const is_mutable = self.current_token.kind == .Var;
        self.advance(); // Consume 'var' or 'const'

        if (self.current_token.kind != .Identifier) {
            return Error.ExpectedIdentifier;
        }

        const var_name = self.current_token.lexeme;
        self.advance(); // Consume identifier

        // Optional type annotation
        var var_type: Type = .Auto;
        if (self.current_token.kind == .Colon) {
            self.advance(); // Consume ':'

            // Parse the type
            switch (self.current_token.kind) {
                .IntType => var_type = .Int,
                .FloatType => var_type = .Float,
                .StringType => var_type = .String,
                .BoolType => var_type = .Bool,
                else => return Error.ExpectedType,
            }
            self.advance(); // Consume type
        }

        var expr: *Node = undefined;

        if (self.current_token.kind == .Equal) {
            self.advance(); // Consume '='
            expr = try self.parseExpression();
        } else {
            // No initializer provided
            if (is_mutable) {   
                // set the value to 'nothing'
                expr = try self.allocator.create(Node);
                expr.* = Node{ .Nothing = .{} };
            } else {
                return Error.EmptyConstantDeclaration;
            }
        }

        // If no explicit type but has initializer, infer type from expression
        if (var_type == .Auto and expr.* != .Nothing) {
            var_type = expr.typ();
        }

        // Handle deep copy for arrays
        if (expr.* == .Array) {
            const array_elements = expr.Array.elements;
            var new_elements = try self.allocator.alloc(*Node, array_elements.len);
            var i: usize = 0;
            for (array_elements) |el| {
                new_elements[i] = try self.deepCopy(el);
                i += 1;
            }
            expr = try self.allocator.create(Node);
            expr.* = .{ .Array = .{ .elements = new_elements, .typ = .Array } };
        }

        const new_node = try self.allocator.create(Node);
        new_node.* = Node{
            .Declaration = .{
                .name = var_name,
                .value = expr,
                .typ = var_type,
                .is_mutable = is_mutable,
            },
        };
        return new_node;
    }

    fn parseAssignment(self: *Parser) !*Node {
        const var_name = self.current_token.lexeme;
        self.advance(); // Consume identifier

        if (self.current_token.kind != .Equal) {
            return Error.ExpectedEqual;
        }
        self.advance(); // Consume '='

        const expr = try self.parseExpression();
        const new_node = try self.allocator.create(Node);
        new_node.* = Node{
            .Assignment = .{
                .name = var_name,
                .value = expr,
                .typ = .Auto,
            },
        };
        return new_node;
    }

    pub fn parseExpression(self: *Parser) Error!*Node {
        return self.parseLogical();
    }

    fn parseLogical(self: *Parser) Error!*Node {
        var node = try self.parseEquality();

        while (self.current_token.kind == .And or self.current_token.kind == .Or) {
            const op = self.current_token.kind;
            self.advance();
            const right = try self.parseEquality();
            const new_node = try self.allocator.create(Node);
            new_node.* = Node{
                .Binary = .{
                    .left = node,
                    .operator = op,
                    .right = right,
                    .typ = .Bool,
                },
            };
            node = new_node;
        }
        return node;
    }

    fn parseEquality(self: *Parser) Error!*Node {
        var node = try self.parseTerm();

        while (self.current_token.kind == .EqualEqual or self.current_token.kind == .NotEqual) {
            const op = self.current_token.kind;
            self.advance();
            const right = try self.parseTerm();
            const new_node = try self.allocator.create(Node);
            new_node.* = Node{
                .Binary = .{
                    .left = node,
                    .operator = op,
                    .right = right,
                    .typ = .Bool,
                },
            };
            node = new_node;
        }
        return node;
    }

    fn parseTerm(self: *Parser) Error!*Node {
        var node = try self.parseFactor();

        while (self.current_token.kind == .Plus or self.current_token.kind == .Minus) {
            const op = self.current_token.kind;
            self.advance();
            const right = try self.parseFactor();
            const new_node = try self.allocator.create(Node);
            new_node.* = Node{
                .Binary = .{
                    .left = node,
                    .operator = op,
                    .right = right,
                    .typ = if (node.typ() == .Float or right.typ() == .Float) Type.Float else Type.Int,
                },
            };
            node = new_node;
        }
        return node;
    }

    fn parseFactor(self: *Parser) Error!*Node {
        var node = try self.parsePrimary();

        while (self.current_token.kind == .Star or self.current_token.kind == .Slash) {
            const op = self.current_token.kind;
            self.advance();
            const right = try self.parsePrimary();
            const new_node = try self.allocator.create(Node);
            new_node.* = Node{
                .Binary = .{
                    .left = node,
                    .operator = op,
                    .right = right,
                    // this will force the type to be float if any of the operands is a float
                    .typ = if (node.typ() == .Float or right.typ() == .Float or op == .Slash) Type.Float else Type.Int,
                },
            };
            node = new_node;
        }
        return node;
    }

    fn parsePrimary(self: *Parser) Error!*Node {
        switch (self.current_token.kind) {
            .Number => {
                const lexeme = self.current_token.lexeme;
                const new_node = try self.allocator.create(Node);

                // Check if the lexeme contains a decimal point
                if (std.mem.indexOfScalar(u8, lexeme, '.')) |_| {
                    // Parse as float
                    const value = std.fmt.parseFloat(f64, lexeme) catch |err| switch (err) {
                        error.InvalidCharacter => return Error.InvalidNumber,
                        else => unreachable,
                    };
                    new_node.* = Node{
                        .Float = .{
                            .value = value,
                            .typ = Type.Float,
                        },
                    };
                } else {
                    // Parse as integer
                    const value = std.fmt.parseInt(i64, lexeme, 10) catch |err| switch (err) {
                        error.InvalidCharacter => return Error.InvalidNumber,
                        else => unreachable,
                    };
                    new_node.* = Node{
                        .Int = .{
                            .value = value,
                            .typ = Type.Int,
                        },
                    };
                }
                self.advance();
                return new_node;
            },
            .Identifier => {
                const name = self.current_token.lexeme;
                const new_node = try self.allocator.create(Node);
                new_node.* = Node{
                    .Variable = .{
                        .name = name,
                        .typ = .Auto,
                    },
                };
                self.advance();
                return new_node;
            },
            .LeftParen => {
                self.advance(); // Consume '('
                const expr = try self.parseExpression();
                if (self.current_token.kind != .RightParen) {
                    return Error.MissingClosingParenthesis;
                }
                self.advance(); // Consume ')'
                return expr;
            },
            .String => {
                const value = self.current_token.lexeme;
                // Duplicate the string to ensure it remains valid
                const duped_value = try self.allocator.dupe(u8, value);

                const new_node = try self.allocator.create(Node);
                new_node.* = Node{
                    .String = .{
                        .value = duped_value,
                        .typ = .String,
                    },
                };
                self.advance();
                return new_node;
            },
            .Nothing => {
                const new_node = try self.allocator.create(Node);
                new_node.* = Node{ .Nothing = .{} };
                self.advance();
                return new_node;
            },
            .True => {
                const new_node = try self.allocator.create(Node);
                new_node.* = Node{ .True = .{} };
                self.advance();
                return new_node;
            },
            .False => {
                const new_node = try self.allocator.create(Node);
                new_node.* = Node{ .False = .{} };
                self.advance();
                return new_node;
            },
            .LeftBracket => {
                self.advance(); // Consume '['
                var elements = ArrayList(*Node).init(self.allocator.*);

                while (!self.check(.RightBracket) and !self.check(.EOF)) {
                    const element = try self.parseExpression();
                    try elements.append(element);

                    if (!self.match(.Comma) and !self.check(.RightBracket)) {
                        return Error.UnexpectedToken;
                    }
                }

                if (!self.match(.RightBracket)) {
                    return Error.UnmatchedBrace;
                }

                const array_node = try self.allocator.create(Node);
                array_node.* = Node{
                    .Array = .{
                        .elements = try elements.toOwnedSlice(),
                        .typ = .Array,
                    },
                };
                return array_node;
            },
            else => {
                return Error.UnexpectedToken;
            },
        }
    }

    fn parseBlock(self: *Parser) Error!*Node {
        var statements = ArrayList(*Node).init(self.allocator.*);

        while (!self.check(.RightBrace) and !self.check(.EOF)) {
            const stmt = try self.parseStatement();
            try statements.append(stmt);
        }

        if (!self.match(.RightBrace)) {
            return Error.UnmatchedBrace;
        }

        const block_node = try self.allocator.create(Node);
        block_node.* = .{ .Block = .{
            .statements = try statements.toOwnedSlice(),
        } };
        return block_node;
    }

    fn deepCopy(self: *Parser, node: *Node) !*Node {
        // Implement the logic to deep copy a node
        const new_node = try self.allocator.create(Node);
        // Assuming Node has a method or logic to copy itself
        new_node.* = node.*; // This is a placeholder; actual implementation may vary
        return new_node;
    }
};
