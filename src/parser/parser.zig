const ast = @import("../ast/ast.zig");
const parser_types = @import("parser_types.zig");
const Errors = @import("../utils/errors.zig");
const ErrorList = Errors.ErrorList;
const Parser = @import("parser_types.zig").Parser;

pub fn parse(self: *Parser) ErrorList![]ast.Stmt {
    return parser_types.execute(self);
}
