const std = @import("std");
const llvm = @import("llvm");
const ast = @import("../ast/ast.zig");
const LLVMGenerator = @import("../codegen/llvmir/llvm.zig").LLVMGenerator;
const TokenImport = @import("../types/token.zig");
const Token = TokenImport.Token;
const TokenType = TokenImport.TokenType;

pub const ASTReader = struct {
    allocator: std.mem.Allocator,
    generator: *LLVMGenerator,
    lines: std.mem.SplitIterator(u8),
    current_line: usize,

    pub fn init(allocator: std.mem.Allocator, generator: *LLVMGenerator, content: []const u8) !ASTReader {
        return ASTReader{
            .allocator = allocator,
            .generator = generator,
            .lines = std.mem.split(u8, content, "\n"),
            .current_line = 0,
        };
    }

    pub fn readAST(self: *ASTReader) !*ast.Expr {
        const root_expr = try self.readNextExpression();
        try self.generator.generateAST(root_expr);
        return root_expr;
    }

    fn readNextExpression(self: *ASTReader) !*ast.Expr {
        const expr_type = try self.readLine();
        return switch (std.meta.stringToEnum(ExprType, expr_type) orelse .Unknown) {
            .Literal => try self.readLiteral(),
            .Function => try self.readFunction(),
            .Expression => try self.readExpression(),
            .VarDecl => try self.readVarDecl(),
            .Return => try self.readReturn(),
            .Binary => try self.readBinary(),
            .Peek => try self.readPeek(),
            else => error.UnsupportedExpressionType,
        };
    }

    fn readLiteral(self: *ASTReader) !*ast.Expr {
        const value_line = try self.readLine();
        const value_parts = std.mem.split(u8, value_line, ":");
        _ = value_parts.next();
        const type_str = value_parts.next() orelse return error.InvalidFormat;
        const value_str = value_parts.next() orelse return error.InvalidFormat;

        const expr = try self.allocator.create(ast.Expr);
        expr.* = switch (std.meta.stringToEnum(LiteralType, type_str) orelse .Unknown) {
            .int => .{ .Literal = .{ .int = try std.fmt.parseInt(i32, value_str, 10) } },
            .string => .{ .Literal = .{ .string = try self.allocator.dupe(u8, value_str) } },
            .float => .{ .Literal = .{ .float = try std.fmt.parseFloat(f64, value_str) } },
            .byte => .{ .Literal = .{ .byte = try std.fmt.parseInt(u8, value_str, 10) } },
            else => return error.UnsupportedLiteralType,
        };
        return expr;
    }

    fn readFunction(self: *ASTReader) !*ast.Expr {
        const name = try self.readProperty("name");
        const is_entry = try self.readBoolProperty("is_entry");
        const params_count = try self.readIntegerProperty("params_count");

        var params = try self.allocator.alloc(ast.FunctionParam, params_count);
        var i: usize = 0;
        while (i < params_count) : (i += 1) {
            const param_name = try self.readProperty("param_name");
            params[i] = .{
                .name = try self.createToken(.IDENTIFIER, param_name),
                .type_expr = null,
            };
        }

        const body_size = try self.readIntegerProperty("body_size");
        var statements = try self.allocator.alloc(ast.Stmt, body_size);
        i = 0;
        while (i < body_size) : (i += 1) {
            statements[i] = try self.readStatement();
        }

        const func = try self.allocator.create(ast.Expr);
        func.* = .{ .Function = .{
            .name = try self.createToken(.IDENTIFIER, name),
            .params = params,
            .return_type_info = .{ .base = .Int },
            .body = statements,
            .is_entry = is_entry,
        } };
        return func;
    }

    fn readStatement(self: *ASTReader) !ast.Stmt {
        const stmt_type = try self.readLine();
        return switch (std.meta.stringToEnum(StmtType, stmt_type) orelse .Unknown) {
            .Expression => .{ .Expression = try self.readNextExpression() },
            .Return => .{ .Return = .{
                .value = try self.readNextExpression(),
                .type_info = .{ .base = .Int },
            } },
            .VarDecl => .{ .VarDecl = try self.readVarDeclStmt() },
            else => error.UnsupportedStatementType,
        };
    }

    fn readVarDeclStmt(self: *ASTReader) !ast.VarDecl {
        const name = try self.readProperty("name");
        const type_str = try self.readProperty("type");
        const is_public = try self.readBoolProperty("is_public");
        const has_initializer = try self.readBoolProperty("has_initializer");

        var initializer: ?*ast.Expr = null;
        if (has_initializer) {
            initializer = try self.readNextExpression();
        }

        return .{
            .name = try self.createToken(.IDENTIFIER, name),
            .type_info = .{ .base = try self.parseType(type_str) },
            .is_public = is_public,
            .initializer = initializer,
        };
    }

    fn readPeek(self: *ASTReader) !*ast.Expr {
        const expr = try self.readNextExpression();
        const peek = try self.allocator.create(ast.Expr);
        peek.* = .{ .Peek = .{
            .expr = expr,
            .location = .{
                .file = "main",
                .range = .{
                    .start_line = self.current_line,
                    .start_col = 1,
                    .end_line = self.current_line,
                    .end_col = 1,
                },
            },
            .variable_name = null,
        } };
        return peek;
    }

    fn readLine(self: *ASTReader) ![]const u8 {
        self.current_line += 1;
        return self.lines.next() orelse return error.UnexpectedEndOfFile;
    }

    fn readInteger(self: *ASTReader) !usize {
        const line = try self.readLine();
        return std.fmt.parseInt(usize, line, 10);
    }

    fn readProperty(self: *ASTReader, expected: []const u8) ![]const u8 {
        const line = try self.readLine();
        var parts = std.mem.split(u8, line, ":");
        const key = parts.next() orelse return error.InvalidFormat;
        if (!std.mem.eql(u8, key, expected)) {
            return error.UnexpectedProperty;
        }
        return parts.next() orelse return error.InvalidFormat;
    }

    fn readIntegerProperty(self: *ASTReader, name: []const u8) !usize {
        const value = try self.readProperty(name);
        return std.fmt.parseInt(usize, value, 10);
    }

    fn readBoolProperty(self: *ASTReader, name: []const u8) !bool {
        const value = try self.readProperty(name);
        return std.mem.eql(u8, value, "true");
    }

    fn createToken(self: *ASTReader, token_type: TokenType, lexeme: []const u8) !Token {
        return Token{
            .type = token_type,
            .lexeme = try self.allocator.dupe(u8, lexeme),
            .literal = .{ .nothing = {} },
            .line = self.current_line,
            .column = 1,
        };
    }

    fn parseType(self: *ASTReader, type_str: []const u8) !ast.TypeInfo.BaseType {
        _ = self;
        return std.meta.stringToEnum(ast.TypeInfo.BaseType, type_str) orelse .Unknown;
    }
};

const ExprType = enum {
    Literal,
    Function,
    Expression,
    VarDecl,
    Return,
    Binary,
    Peek,
    Unknown,
};

const StmtType = enum {
    Expression,
    Return,
    VarDecl,
    Unknown,
};

const LiteralType = enum {
    int,
    string,
    float,
    byte,
    Unknown,
};

pub fn readASTFromFile(allocator: std.mem.Allocator, generator: *LLVMGenerator, file_path: []const u8) !*ast.Expr {
    const file = try std.fs.cwd().openFile(file_path, .{});
    defer file.close();

    const content = try file.readToEndAlloc(allocator, std.math.maxInt(usize));
    defer allocator.free(content);

    var reader = try ASTReader.init(allocator, generator, content);
    return try reader.readAST();
}
