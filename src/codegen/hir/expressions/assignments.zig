const ast = @import("../../../ast/ast.zig");
const HIRGenerator = @import("../soxa_generator.zig").HIRGenerator;
const SoxaTypes = @import("../soxa_types.zig");

/// Lower an assignment to a name. A compound assignment is parsed as one.
pub const AssignmentsHandler = struct {
    generator: *HIRGenerator,

    pub fn init(generator: *HIRGenerator) AssignmentsHandler {
        return .{ .generator = generator };
    }

    /// Generate HIR for assignment expressions
    pub fn generateAssignment(self: *AssignmentsHandler, expr: *ast.Expr, preserve_result: bool) !void {
        const assign = expr.data.Assignment;
        const value = assign.value.?;
        try self.generator.generateExpression(value, true, false);

        // The value takes the type of the storage it lands in, which the
        // analyzer resolved beneath any narrowing of the name.
        const assigned_type = try self.generator.bindingTypeOf(&expr.base);
        try self.generator.convertValue(try self.generator.typeOf(value), assigned_type);

        // Duplicate value to leave it on stack as assignment result
        if (preserve_result) {
            try self.generator.instructions.append(.Dup);
        }
        try self.generator.storeName(&expr.base, assign.name.lexeme, assigned_type, .rehome);
    }
};
