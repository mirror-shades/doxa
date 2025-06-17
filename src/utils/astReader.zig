const std = @import("std");
const ast = @import("../ast/ast.zig");

// Add this new function to print statements
pub fn printStatement(stmt: ast.Stmt, indent: usize) void {
    var indent_str: []const u8 = "";
    var i: usize = 0;
    while (i < indent) : (i += 1) {
        indent_str = "  ";
    }

    switch (stmt) {
        .VarDecl => |v| {
            std.debug.print("{s}VarDecl: {s}\n", .{ indent_str, v.name.lexeme });
            if (v.initializer) |init| {
                std.debug.print("{s}  Initializer:\n", .{indent_str});
                printExpression(init, indent + 2);
            }
        },
        .Function => |f| {
            std.debug.print("{s}Function: {s}\n", .{ indent_str, f.name.lexeme });
            std.debug.print("{s}  Parameters:\n", .{indent_str});
            for (f.params) |param| {
                std.debug.print("{s}    {s}\n", .{ indent_str, param.name.lexeme });
            }
            std.debug.print("{s}  Body:\n", .{indent_str});
            for (f.body) |body_stmt| {
                printStatement(body_stmt, indent + 2);
            }
        },
        .Expression => |expr| {
            if (expr) |e| {
                std.debug.print("{s}Expression:\n", .{indent_str});
                printExpression(e, indent + 1);
            }
        },
        .Block => |block| {
            std.debug.print("{s}Block:\n", .{indent_str});
            for (block) |block_stmt| {
                printStatement(block_stmt, indent + 1);
            }
        },
        .Module => |m| {
            std.debug.print("{s}Module: {s}\n", .{ indent_str, m.name.lexeme });
        },
        .Import => |imp| {
            std.debug.print("{s}Import: {s}\n", .{ indent_str, imp.module_path });
        },
        .Path => |p| {
            std.debug.print("{s}Path: {s}\n", .{ indent_str, p });
        },
        .Continue => {
            std.debug.print("{s}Continue\n", .{indent_str});
        },
        .Break => {
            std.debug.print("{s}Break\n", .{indent_str});
        },
        .Assert => |a| {
            std.debug.print("{s}Assert:\n", .{indent_str});
            printExpression(a.condition, indent + 1);
            if (a.message) |msg| {
                std.debug.print("{s}  Message:\n", .{indent_str});
                printExpression(msg, indent + 2);
            }
        },
        .Return => |ret| {
            std.debug.print("{s}Return:\n", .{indent_str});
            if (ret.value) |value| {
                printExpression(value, indent + 1);
            }
        },
        .EnumDecl => |e| {
            std.debug.print("{s}EnumDecl: {s}\n", .{ indent_str, e.name.lexeme });
        },
        .Map => |entries| {
            std.debug.print("{s}Map:\n", .{indent_str});
            for (entries) |entry| {
                std.debug.print("{s}  Entry:\n", .{indent_str});
                printExpression(entry.key, indent + 2);
                printExpression(entry.value, indent + 2);
            }
        },
        .Try => |t| {
            std.debug.print("{s}Try:\n", .{indent_str});
            std.debug.print("{s}  Try Body:\n", .{indent_str});
            for (t.try_body) |try_stmt| {
                printStatement(try_stmt, indent + 2);
            }
            std.debug.print("{s}  Catch Body:\n", .{indent_str});
            for (t.catch_body) |catch_stmt| {
                printStatement(catch_stmt, indent + 2);
            }
        },
    }
}

// Add this new function to print expressions
pub fn printExpression(expr: *ast.Expr, indent: usize) void {
    var indent_str: []const u8 = "";
    var i: usize = 0;
    while (i < indent) : (i += 1) {
        indent_str = "  ";
    }

    switch (expr.*) {
        .Literal => |lit| {
            std.debug.print("{s}Literal: {any}\n", .{ indent_str, lit });
        },
        .Binary => |bin| {
            std.debug.print("{s}Binary: {s}\n", .{ indent_str, bin.operator.lexeme });
            if (bin.left) |left| {
                std.debug.print("{s}  Left:\n", .{indent_str});
                printExpression(left, indent + 2);
            }
            if (bin.right) |right| {
                std.debug.print("{s}  Right:\n", .{indent_str});
                printExpression(right, indent + 2);
            }
        },
        .Unary => |un| {
            std.debug.print("{s}Unary: {s}\n", .{ indent_str, un.operator.lexeme });
            if (un.right) |right| {
                printExpression(right, indent + 1);
            }
        },
        .Inspect => |insp| {
            std.debug.print("{s}Inspect:\n", .{indent_str});
            printExpression(insp.expr, indent + 1);
        },
        .Input => |input| {
            std.debug.print("{s}Input: {s}\n", .{ indent_str, input.prompt.lexeme });
        },
        .Variable => |var_token| {
            std.debug.print("{s}Variable: {s}\n", .{ indent_str, var_token.lexeme });
        },
        .Assignment => |assign| {
            std.debug.print("{s}Assignment: {s}\n", .{ indent_str, assign.name.lexeme });
            if (assign.value) |value| {
                std.debug.print("{s}  Value:\n", .{indent_str});
                printExpression(value, indent + 2);
            }
        },
        .Grouping => |group| {
            std.debug.print("{s}Grouping:\n", .{indent_str});
            if (group) |group_expr| {
                printExpression(group_expr, indent + 1);
            }
        },
        .If => |if_expr| {
            std.debug.print("{s}If Expression:\n", .{indent_str});
            if (if_expr.condition) |condition| {
                std.debug.print("{s}  Condition:\n", .{indent_str});
                printExpression(condition, indent + 2);
            }
            if (if_expr.then_branch) |then_branch| {
                std.debug.print("{s}  Then:\n", .{indent_str});
                printExpression(then_branch, indent + 2);
            }
            if (if_expr.else_branch) |else_branch| {
                std.debug.print("{s}  Else:\n", .{indent_str});
                printExpression(else_branch, indent + 2);
            }
        },
        .Block => |block| {
            std.debug.print("{s}Block Expression:\n", .{indent_str});
            for (block.statements) |stmt| {
                printStatement(stmt, indent + 1);
            }
            if (block.value) |value| {
                std.debug.print("{s}  Value:\n", .{indent_str});
                printExpression(value, indent + 1);
            }
        },
        .Array => |arr| {
            std.debug.print("{s}Array:\n", .{indent_str});
            for (arr, 0..) |elem, j| {
                std.debug.print("{s}  Element {}:\n", .{ indent_str, j });
                printExpression(elem, indent + 2);
            }
        },
        .Tuple => |tuple| {
            std.debug.print("{s}Tuple:\n", .{indent_str});
            for (tuple, 0..) |elem, j| {
                std.debug.print("{s}  Element {}:\n", .{ indent_str, j });
                printExpression(elem, indent + 2);
            }
        },
        .Struct => |fields| {
            std.debug.print("{s}Struct:\n", .{indent_str});
            for (fields) |field| {
                std.debug.print("{s}  Field {s}:\n", .{ indent_str, field.name.lexeme });
                printExpression(field.value, indent + 2);
            }
        },
        .Index => |index| {
            std.debug.print("{s}Index:\n", .{indent_str});
            std.debug.print("{s}  Array:\n", .{indent_str});
            printExpression(index.array, indent + 2);
            std.debug.print("{s}  Index:\n", .{indent_str});
            printExpression(index.index, indent + 2);
        },
        .IndexAssign => |index_assign| {
            std.debug.print("{s}Index Assignment:\n", .{indent_str});
            std.debug.print("{s}  Array:\n", .{indent_str});
            printExpression(index_assign.array, indent + 2);
            std.debug.print("{s}  Index:\n", .{indent_str});
            printExpression(index_assign.index, indent + 2);
            std.debug.print("{s}  Value:\n", .{indent_str});
            printExpression(index_assign.value, indent + 2);
        },
        .Call => |call| {
            std.debug.print("{s}Call:\n", .{indent_str});
            std.debug.print("{s}  Callee:\n", .{indent_str});
            printExpression(call.callee, indent + 2);
            std.debug.print("{s}  Arguments:\n", .{indent_str});
            for (call.arguments) |arg| {
                printExpression(arg, indent + 2);
            }
        },
        .Logical => |logical| {
            std.debug.print("{s}Logical: {s}\n", .{ indent_str, logical.operator.lexeme });
            std.debug.print("{s}  Left:\n", .{indent_str});
            printExpression(logical.left, indent + 2);
            std.debug.print("{s}  Right:\n", .{indent_str});
            printExpression(logical.right, indent + 2);
        },
        .Function => |func| {
            std.debug.print("{s}Function Expression: {s}\n", .{ indent_str, func.name.lexeme });
            std.debug.print("{s}  Parameters:\n", .{indent_str});
            for (func.params) |param| {
                std.debug.print("{s}    {s}\n", .{ indent_str, param.name.lexeme });
            }
            std.debug.print("{s}  Body:\n", .{indent_str});
            for (func.body) |stmt| {
                printStatement(stmt, indent + 2);
            }
        },
        .While => |while_expr| {
            std.debug.print("{s}While Expression:\n", .{indent_str});
            std.debug.print("{s}  Condition:\n", .{indent_str});
            printExpression(while_expr.condition, indent + 2);
            std.debug.print("{s}  Body:\n", .{indent_str});
            printExpression(while_expr.body, indent + 2);
        },
        .For => |for_expr| {
            std.debug.print("{s}For Expression:\n", .{indent_str});
            if (for_expr.initializer) |init| {
                std.debug.print("{s}  Initializer:\n", .{indent_str});
                printStatement(init.*, indent + 2);
            }
            if (for_expr.condition) |cond| {
                std.debug.print("{s}  Condition:\n", .{indent_str});
                printExpression(cond, indent + 2);
            }
            if (for_expr.increment) |inc| {
                std.debug.print("{s}  Increment:\n", .{indent_str});
                printExpression(inc, indent + 2);
            }
            std.debug.print("{s}  Body:\n", .{indent_str});
            printExpression(for_expr.body, indent + 2);
        },
        .ForEach => |foreach| {
            std.debug.print("{s}ForEach Expression:\n", .{indent_str});
            std.debug.print("{s}  Item: {s}\n", .{ indent_str, foreach.item_name.lexeme });
            std.debug.print("{s}  Array:\n", .{indent_str});
            printExpression(foreach.array, indent + 2);
            std.debug.print("{s}  Body:\n", .{indent_str});
            for (foreach.body) |stmt| {
                printStatement(stmt, indent + 2);
            }
        },
        .FieldAccess => |field| {
            std.debug.print("{s}Field Access: {s}\n", .{ indent_str, field.field.lexeme });
            std.debug.print("{s}  Object:\n", .{indent_str});
            printExpression(field.object, indent + 2);
        },
        .StructDecl => |struct_decl| {
            std.debug.print("{s}Struct Declaration: {s}\n", .{ indent_str, struct_decl.name.lexeme });
            std.debug.print("{s}  Fields:\n", .{indent_str});
            for (struct_decl.fields) |field| {
                std.debug.print("{s}    {s}\n", .{ indent_str, field.name.lexeme });
            }
        },
        .StructLiteral => |struct_lit| {
            std.debug.print("{s}Struct Literal: {s}\n", .{ indent_str, struct_lit.name.lexeme });
            for (struct_lit.fields) |field| {
                std.debug.print("{s}  Field {s}:\n", .{ indent_str, field.name.lexeme });
                printExpression(field.value, indent + 2);
            }
        },
        .FieldAssignment => |field_assign| {
            std.debug.print("{s}Field Assignment: {s}\n", .{ indent_str, field_assign.field.lexeme });
            std.debug.print("{s}  Object:\n", .{indent_str});
            printExpression(field_assign.object, indent + 2);
            std.debug.print("{s}  Value:\n", .{indent_str});
            printExpression(field_assign.value, indent + 2);
        },
        .Exists => |exists| {
            std.debug.print("{s}Exists: {s}\n", .{ indent_str, exists.variable.lexeme });
            std.debug.print("{s}  Array:\n", .{indent_str});
            printExpression(exists.array, indent + 2);
            std.debug.print("{s}  Condition:\n", .{indent_str});
            printExpression(exists.condition, indent + 2);
        },
        .ForAll => |forall| {
            std.debug.print("{s}ForAll: {s}\n", .{ indent_str, forall.variable.lexeme });
            std.debug.print("{s}  Array:\n", .{indent_str});
            printExpression(forall.array, indent + 2);
            std.debug.print("{s}  Condition:\n", .{indent_str});
            printExpression(forall.condition, indent + 2);
        },
        .ArrayType => |array_type| {
            std.debug.print("{s}Array Type:\n", .{indent_str});
            // Note: element_type is a TypeExpr, not an Expr, so we can't print it with printExpression
            if (array_type.size) |size| {
                std.debug.print("{s}  Size:\n", .{indent_str});
                printExpression(size, indent + 2);
            }
        },
        .Match => |match| {
            std.debug.print("{s}Match:\n", .{indent_str});
            std.debug.print("{s}  Value:\n", .{indent_str});
            printExpression(match.value, indent + 2);
            std.debug.print("{s}  Cases:\n", .{indent_str});
            for (match.cases) |case| {
                std.debug.print("{s}    Pattern: {s}\n", .{ indent_str, case.pattern.lexeme });
                std.debug.print("{s}    Body:\n", .{indent_str});
                printExpression(case.body, indent + 3);
            }
        },
        .EnumDecl => |enum_decl| {
            std.debug.print("{s}Enum Declaration: {s}\n", .{ indent_str, enum_decl.name.lexeme });
            std.debug.print("{s}  Variants:\n", .{indent_str});
            for (enum_decl.variants) |variant| {
                std.debug.print("{s}    {s}\n", .{ indent_str, variant.lexeme });
            }
        },
        .EnumMember => |member| {
            std.debug.print("{s}Enum Member: {s}\n", .{ indent_str, member.lexeme });
        },
        .DefaultArgPlaceholder => {
            std.debug.print("{s}Default Argument Placeholder\n", .{indent_str});
        },
        .TypeOf => |typeof_expr| {
            std.debug.print("{s}TypeOf:\n", .{indent_str});
            printExpression(typeof_expr, indent + 1);
        },
        .Map => |entries| {
            std.debug.print("{s}Map:\n", .{indent_str});
            for (entries) |entry| {
                std.debug.print("{s}  Entry:\n", .{indent_str});
                std.debug.print("{s}    Key:\n", .{indent_str});
                printExpression(entry.key, indent + 3);
                std.debug.print("{s}    Value:\n", .{indent_str});
                printExpression(entry.value, indent + 3);
            }
        },
        .MethodCall => |method| {
            std.debug.print("{s}Method Call: {s}\n", .{ indent_str, method.method.lexeme });
            std.debug.print("{s}  Receiver:\n", .{indent_str});
            printExpression(method.receiver, indent + 2);
            std.debug.print("{s}  Arguments:\n", .{indent_str});
            for (method.arguments) |arg| {
                printExpression(arg, indent + 2);
            }
        },
        .ArrayPush => |arr_push| {
            std.debug.print("{s}Array Push:\n", .{indent_str});
            std.debug.print("{s}  Array:\n", .{indent_str});
            printExpression(arr_push.array, indent + 2);
            std.debug.print("{s}  Element:\n", .{indent_str});
            printExpression(arr_push.element, indent + 2);
        },
        .ArrayLength => |arr_len| {
            std.debug.print("{s}Array Length:\n", .{indent_str});
            printExpression(arr_len.array, indent + 1);
        },
        .ArrayPop => |arr_pop| {
            std.debug.print("{s}Array Pop:\n", .{indent_str});
            printExpression(arr_pop.array, indent + 1);
        },
        .ArrayIsEmpty => |arr_empty| {
            std.debug.print("{s}Array Is Empty:\n", .{indent_str});
            printExpression(arr_empty.array, indent + 1);
        },
        .ArrayConcat => |arr_concat| {
            std.debug.print("{s}Array Concat:\n", .{indent_str});
            std.debug.print("{s}  Array 1:\n", .{indent_str});
            printExpression(arr_concat.array, indent + 2);
            std.debug.print("{s}  Array 2:\n", .{indent_str});
            printExpression(arr_concat.array2, indent + 2);
        },
        .CompoundAssign => |compound| {
            std.debug.print("{s}Compound Assignment: {s}\n", .{ indent_str, compound.operator.lexeme });
            std.debug.print("{s}  Name: {s}\n", .{ indent_str, compound.name.lexeme });
            if (compound.value) |value| {
                std.debug.print("{s}  Value:\n", .{indent_str});
                printExpression(value, indent + 2);
            }
        },
        .Assert => |assert| {
            std.debug.print("{s}Assert:\n", .{indent_str});
            printExpression(assert.condition, indent + 1);
            if (assert.message) |msg| {
                std.debug.print("{s}  Message:\n", .{indent_str});
                printExpression(msg, indent + 2);
            }
        },
    }
}
