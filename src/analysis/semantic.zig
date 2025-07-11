const Ast = @import("../ast/ast.zig").Ast;

const SemanticAnalyzer = struct {
    pub fn analyze(self: *SemanticAnalyzer, ast: *Ast) Ast {
        _ = self;
        return ast;
    }
};
