const std = @import("std");
const Allocator = std.mem.Allocator;
const ast = @import("../ast/ast.zig");
const token = @import("../lexer/token.zig");
const Reporting = @import("../utils/reporting.zig");
const Parser = @import("../parser/parser_types.zig").Parser;

pub const Transpiler = struct {
    allocator: Allocator,
    parser: *Parser,
    debug_enabled: bool,
    output: std.ArrayList(u8),

    pub fn init(allocator: Allocator, parser: *Parser, debug_enabled: bool) Transpiler {
        return Transpiler{
            .allocator = allocator,
            .parser = parser,
            .debug_enabled = debug_enabled,
            .output = std.ArrayList(u8).init(allocator),
        };
    }

    pub fn deinit(self: *Transpiler) void {
        self.output.deinit();
    }

    fn writeAll(self: *Transpiler, text: []const u8) Reporting.ErrorList!void {
        try self.output.appendSlice(text);
    }

    fn transpileVarDeclFields(self: *Transpiler, var_decl: @TypeOf(@as(ast.Stmt, undefined).VarDecl)) Reporting.ErrorList!void {
        // Write visibility
        if (var_decl.is_public) {
            try self.writeAll("pub ");
        }

        // Write mutability
        if (var_decl.type_info.is_mutable) {
            try self.writeAll("var ");
        } else {
            try self.writeAll("const ");
        }

        // Write variable name
        try self.writeAll(var_decl.name.lexeme);
        
        // Write type annotation if not auto
        if (var_decl.type_info.base != .Auto) {
            try self.writeAll(": ");
            switch (var_decl.type_info.base) {
                .Int => try self.writeAll("i32"),
                .Float => try self.writeAll("f64"),
                .String => try self.writeAll("[]const u8"),
                .Boolean => try self.writeAll("bool"),
                else => try self.writeAll("void"), // placeholder for other types
            }
        }

        // Write initializer
        try self.writeAll(" = ");
        try self.transpileExpr(var_decl.initializer);
        try self.writeAll(";\n");
    }

    fn transpileInspect(self: *Transpiler, expr: *ast.Expr, variable_name: ?[]const u8, location: ast.Location) Reporting.ErrorList!void {
        // Convert ast.Location to Reporting.Reporter.Location
        const reporter_location = Reporting.Reporter.Location{
            .file = location.file,
            .line = location.line,
            .column = location.column,
        };
        
        // First evaluate the expression
        try self.writeAll("(blk: {");
        try self.writeAll("\n    const __value = ");
        try self.transpileExpr(expr);
        try self.writeAll(";");
        try self.writeAll("\n    var __buffer = std.ArrayList(u8).init(allocator);");
        try self.writeAll("\n    defer __buffer.deinit();");
        
        // Write location info
        try self.writeAll("\n    try __buffer.writer().print(\"[{s}:{d}:{d}] {s} = \", .{");
        try self.writeAll("\n        \"");
        try self.writeAll(reporter_location.file);
        try self.writeAll("\",");
        try self.writeAll("\n        ");
        try self.writeAll(try std.fmt.allocPrint(self.allocator, "{d}", .{reporter_location.line}));
        try self.writeAll(",");
        try self.writeAll("\n        ");
        try self.writeAll(try std.fmt.allocPrint(self.allocator, "{d}", .{reporter_location.column}));
        try self.writeAll(",");
        try self.writeAll("\n        \"");
        try self.writeAll(variable_name orelse "value");
        try self.writeAll("\"");
        try self.writeAll("\n    });");

        // Write value formatting
        try self.writeAll("\n    switch (__value) {");
        try self.writeAll("\n        .int => |i| try __buffer.writer().print(\"{d}\", .{i}),");
        try self.writeAll("\n        .float => |f| try __buffer.writer().print(\"{d}\", .{f}),");
        try self.writeAll("\n        .string => |s| try __buffer.writer().print(\"\\\"{s}\\\"\", .{s}),");
        try self.writeAll("\n        .boolean => |b| try __buffer.writer().print(\"{s}\", .{@tagName(b)}),");
        try self.writeAll("\n        .array => |arr| {");
        try self.writeAll("\n            try __buffer.writer().print(\"[\", .{});");
        try self.writeAll("\n            for (arr, 0..) |item, i| {");
        try self.writeAll("\n                if (i > 0) try __buffer.writer().print(\", \", .{});");
        try self.writeAll("\n                switch (item) {");
        try self.writeAll("\n                    .int => |n| try __buffer.writer().print(\"{d}\", .{n}),");
        try self.writeAll("\n                    .float => |f| try __buffer.writer().print(\"{d}\", .{f}),");
        try self.writeAll("\n                    .string => |s| try __buffer.writer().print(\"\\\"{s}\\\"\", .{s}),");
        try self.writeAll("\n                    .boolean => |b| try __buffer.writer().print(\"{}\", .{b}),");
        try self.writeAll("\n                    else => try __buffer.writer().print(\"{any}\", .{item}),");
        try self.writeAll("\n                }");
        try self.writeAll("\n            }");
        try self.writeAll("\n            try __buffer.writer().print(\"]\", .{});");
        try self.writeAll("\n        },");
        try self.writeAll("\n        else => try __buffer.writer().print(\"{any}\", .{__value}),");
        try self.writeAll("\n    }");

        try self.writeAll("\n    try __buffer.writer().print(\"\\n\", .{});");
        try self.writeAll("\n    try std.io.getStdOut().writeAll(__buffer.items);");
        try self.writeAll("\n    break :blk __value;");
        try self.writeAll("\n})");
    }

    fn transpileExpr(self: *Transpiler, expr: ?*ast.Expr) Reporting.ErrorList!void {
        if (expr) |e| {
            switch (e.*) {
                .Literal => |lit| {
                    switch (lit) {
                        .int => |i| try self.writeAll(try std.fmt.allocPrint(self.allocator, "{d}", .{i})),
                        .float => |f| try self.writeAll(try std.fmt.allocPrint(self.allocator, "{d}", .{f})),
                        .string => |s| try self.writeAll(s),
                        .boolean => |b| try self.writeAll(if (b == .true) "true" else "false"),
                        else => try self.writeAll("undefined"),
                    }
                },
                .Variable => |var_token| {
                    try self.writeAll(var_token.lexeme);
                },
                .Binary => |bin| {
                    try self.writeAll("(");
                    try self.transpileExpr(bin.left);
                    try self.writeAll(" ");
                    // Map operator
                    const op = switch (bin.operator.type) {
                        .PLUS => "+",
                        .MINUS => "-",
                        .ASTERISK => "*",
                        .SLASH => "/",
                        else => "?",
                    };
                    try self.writeAll(op);
                    try self.writeAll(" ");
                    try self.transpileExpr(bin.right);
                    try self.writeAll(")");
                },
                .Inspect => |inspect_expr| {
                    try self.transpileInspect(inspect_expr.expr, inspect_expr.variable_name, inspect_expr.location);
                },
                else => try self.writeAll("undefined"),
            }
        } else {
            try self.writeAll("undefined");
        }
    }

    pub fn transpile(self: *Transpiler, statements: []ast.Stmt) Reporting.ErrorList![]const u8 {
        if (self.debug_enabled) {
            std.debug.print("Starting transpilation...\n", .{});
        }

        // Add std.debug import at the top
        try self.writeAll("const std = @import(\"std\");\n");
        try self.writeAll("const allocator = std.heap.page_allocator;\n\n");

        for (statements) |statement| {
            switch (statement) {
                .VarDecl => |var_decl| try self.transpileVarDeclFields(var_decl),
                else => {
                    if (self.debug_enabled) {
                        std.debug.print("Unhandled statement type: {any}\n", .{statement});
                    }
                },
            }
        }

        return self.output.toOwnedSlice();
    }
};