const Parser = @import("parser_types.zig").Parser;
const ast = @import("../ast.zig");
const token = @import("../token.zig");

pub fn parseImportStmt(self: *Parser) !ast.Stmt {
    self.advance(); // consume 'import'

    // Parse module path
    if (self.peek().type != .STRING) {
        return error.ExpectedString;
    }
    const module_path = self.peek().lexeme;
    self.advance();

    // Must have 'is' keyword
    if (self.peek().type != .ASSIGN) {
        return error.ExpectedAssignmentOperator;
    }
    self.advance();

    // Parse namespace alone or namespace.symbol
    if (self.peek().type != .IDENTIFIER) {
        return error.ExpectedIdentifier;
    }
    const namespace = self.peek().lexeme;
    self.advance();

    // Check if there's a dot (for selective import)
    var specific_symbol: ?[]const u8 = null;
    if (self.peek().type == .DOT) {
        self.advance(); // consume dot

        // Get the specific symbol
        if (self.peek().type != .IDENTIFIER) {
            return error.ExpectedIdentifier;
        }
        specific_symbol = self.peek().lexeme;
        self.advance();
    }

    // Semicolon check
    if (self.peek().type != .SEMICOLON) {
        return error.ExpectedSemicolon;
    }
    self.advance();

    return ast.Stmt{
        .Import = .{
            .module_path = module_path,
            .namespace_alias = namespace,
            .specific_symbol = specific_symbol,
        },
    };
}
