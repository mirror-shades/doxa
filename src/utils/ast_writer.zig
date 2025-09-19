const std = @import("std");
const ast = @import("../ast/ast.zig");

pub fn writeASTToFile(statements: []const ast.Stmt, file_path: []const u8) !void {
    var file = try std.fs.cwd().createFile(file_path, .{});
    defer file.close();

    var buffer: [1024]u8 = undefined;
    var file_writer = file.writer(&buffer);
    const writer = &file_writer.interface;
    try writeStatements(statements, writer);
    try writer.flush();
}

// Helper function to write source location information
fn writeSourceLocation(base: ast.Base, writer: anytype) anyerror!void {
    try writer.print("node_id:{d}\n", .{base.id});

    if (base.span) |span| {
        // New location format
        try writer.print("start_line:{d}\n", .{span.location.range.start_line});
        try writer.print("start_col:{d}\n", .{span.location.range.start_col});
        try writer.print("end_line:{d}\n", .{span.location.range.end_line});
        try writer.print("end_col:{d}\n", .{span.location.range.end_col});
        try writer.print("file:{s}\n", .{span.location.file});
    } else {
        // Synthetic node without location
        try writer.print("synthetic_node:true\n", .{});
    }
}

// Helper function to write detailed type information
fn writeDetailedTypeInfo(type_info: ast.TypeInfo, writer: anytype) anyerror!void {
    try writer.print("type_base:{s}\n", .{@tagName(type_info.base)});
    try writer.print("is_mutable:{}\n", .{type_info.is_mutable});

    // Custom type names for structs and enums
    if (type_info.custom_type) |custom| {
        try writer.print("custom_type:{s}\n", .{custom});
    }

    // Element type for homogeneous arrays
    if (type_info.element_type) |elem_type| {
        try writer.print("element_type:{s}\n", .{@tagName(elem_type)});
    }

    // Array size if specified
    if (type_info.array_size) |size| {
        try writer.print("array_size:{d}\n", .{size});
    }

    // Struct field information for heterogeneous structs
    if (type_info.struct_fields) |fields| {
        try writer.print("struct_fields_count:{d}\n", .{fields.len});
        for (fields, 0..) |field, i| {
            try writer.print("field_{d}_name:{s}\n", .{ i, field.name });
            try writer.print("field_{d}_type:{s}\n", .{ i, @tagName(field.type_info.base) });
        }
    }

    // Enum variants
    if (type_info.variants) |variants| {
        try writer.print("enum_variants_count:{d}\n", .{variants.len});
        for (variants, 0..) |variant, i| {
            try writer.print("variant_{d}:{s}\n", .{ i, variant });
        }
    }
}

fn writeStatements(statements: []const ast.Stmt, writer: anytype) anyerror!void {
    // Write number of statements
    try writer.print("{d}\n", .{statements.len});

    // Write each statement
    for (statements) |stmt| {
        try writeStatement(stmt, writer);
    }
}

fn writeStatement(stmt: ast.Stmt, writer: anytype) anyerror!void {
    // Write statement type
    try writer.print("{s}\n", .{@tagName(stmt.data)});

    // Write source location information
    try writeSourceLocation(stmt.base, writer);

    switch (stmt.data) {
        .VarDecl => |v| {
            try writer.print("name:{s}\n", .{v.name.lexeme});
            try writeDetailedTypeInfo(v.type_info, writer);
            try writer.print("is_public:{}\n", .{v.is_public});
            if (v.initializer) |init| {
                try writer.print("has_initializer:true\n", .{});
                try writeExpression(init, writer);
            } else {
                try writer.print("has_initializer:false\n", .{});
            }
        },
        .FunctionDecl => |f| {
            try writer.print("name:{s}\n", .{f.name.lexeme});
            try writer.print("is_entry:{}\n", .{f.is_entry});
            try writer.print("is_public:{}\n", .{f.is_public});
            try writer.print("return_type_detailed:\n", .{});
            try writeDetailedTypeInfo(f.return_type_info, writer);
            try writer.print("params_count:{d}\n", .{f.params.len});
            for (f.params) |param| {
                try writer.print("param_name:{s}\n", .{param.name.lexeme});
                if (param.type_expr) |_| {
                    try writer.print("param_has_type:true\n", .{});
                    // Note: type_expr is a TypeExpr, not TypeInfo - would need conversion
                } else {
                    try writer.print("param_has_type:false\n", .{});
                }
            }
            try writeStatements(f.body, writer);
        },
        .Expression => |expr| {
            if (expr) |e| {
                try writer.print("has_expr:true\n", .{});
                try writeExpression(e, writer);
            } else {
                try writer.print("has_expr:false\n", .{});
            }
        },
        .Block => |block| {
            try writeStatements(block, writer);
        },
        .Return => |ret| {
            try writer.print("return_type_detailed:\n", .{});
            try writeDetailedTypeInfo(ret.type_info, writer);
            if (ret.value) |value| {
                try writer.print("has_value:true\n", .{});
                try writeExpression(value, writer);
            } else {
                try writer.print("has_value:false\n", .{});
            }
        },
        .EnumDecl => |e| {
            try writer.print("name:{s}\n", .{e.name.lexeme});
            try writer.print("is_public:{}\n", .{e.is_public});
            try writer.print("variants_count:{d}\n", .{e.variants.len});
            for (e.variants) |variant| {
                try writer.print("variant:{s}\n", .{variant.lexeme});
            }
        },
        .MapLiteral => |entries| {
            try writer.print("map_entries_count:{d}\n", .{entries.len});
            for (entries, 0..) |entry, i| {
                try writer.print("map_entry_{d}_key:\n", .{i});
                try writeExpression(entry.key, writer);
                try writer.print("map_entry_{d}_value:\n", .{i});
                try writeExpression(entry.value, writer);
            }
        },
        else => {
            // For other statement types, just write their tag name
            // Add more specific serialization as needed
        },
    }
}

fn writeExpression(expr: *const ast.Expr, writer: anytype) anyerror!void {
    try writer.print("expr_type:{s}\n", .{@tagName(expr.data)});

    // Write source location information
    try writeSourceLocation(expr.base, writer);

    switch (expr.data) {
        .Literal => |lit| {
            switch (lit) {
                .int => |i| try writer.print("value:int:{d}\n", .{i}),
                .float => |f| try writer.print("value:float:{d}\n", .{f}),
                .string => |s| try writer.print("value:string:{s}\n", .{s}),
                .byte => |b| try writer.print("value:byte:{d}\n", .{b}),
                else => try writer.print("value:other\n", .{}),
            }
        },
        .Binary => |bin| {
            try writer.print("operator:{s}\n", .{bin.operator.lexeme});
            if (bin.left) |left| {
                try writer.print("has_left:true\n", .{});
                try writeExpression(left, writer);
            }
            if (bin.right) |right| {
                try writer.print("has_right:true\n", .{});
                try writeExpression(right, writer);
            }
        },
        .Variable => |var_token| {
            try writer.print("name:{s}\n", .{var_token.lexeme});
        },
        .Array => |elements| {
            try writer.print("array_elements_count:{d}\n", .{elements.len});
            for (elements, 0..) |element, i| {
                try writer.print("element_{d}:\n", .{i});
                try writeExpression(element, writer);
            }
        },
        .StructLiteral => |struct_lit| {
            try writer.print("struct_name:{s}\n", .{struct_lit.name.lexeme});
            try writer.print("struct_fields_count:{d}\n", .{struct_lit.fields.len});
            for (struct_lit.fields, 0..) |field, i| {
                try writer.print("struct_field_{d}_name:{s}\n", .{ i, field.name.lexeme });
                try writer.print("struct_field_{d}_value:\n", .{i});
                try writeExpression(field.value, writer);
            }
        },
        .FunctionCall => |call| {
            try writer.print("args_count:{d}\n", .{call.arguments.len});
            try writeExpression(call.callee, writer);
            for (call.arguments) |arg| {
                try writeExpression(arg.expr, writer);
            }
        },
        .Cast => |_| {
            try writer.print("cast_expression\n", .{});
            // Optionally, print more details about the cast
        },
        .Map => |entries| {
            try writer.print("map_entries_count:{d}\n", .{entries.len});
            for (entries, 0..) |entry, i| {
                try writer.print("map_entry_{d}_key:\n", .{i});
                try writeExpression(entry.key, writer);
                try writer.print("map_entry_{d}_value:\n", .{i});
                try writeExpression(entry.value, writer);
            }
        },
        else => {
            // For other expression types, just write their tag name
            // Add more specific serialization as needed
        },
    }
}
