const std = @import("std");
const testing = std.testing;

const inline_zig = @import("../src/parser/inline_zig.zig");
const ast = @import("../src/ast/ast.zig");

test "inline zig: accepts import consts and function bodies" {
    const src =
        \\const std = @import("std");
        \\
        \\fn add(a: i64, b: i64) i64 {
        \\    return a + b;
        \\}
    ;

    const sigs = try inline_zig.sanitizeAndExtract(testing.allocator, src);
    defer inline_zig.deinitSigs(testing.allocator, sigs);

    try testing.expectEqual(@as(usize, 1), sigs.len);
    try testing.expectEqualStrings("add", sigs[0].name);
    try testing.expectEqual(ast.Type.Int, sigs[0].return_type.base);
    try testing.expectEqual(@as(usize, 2), sigs[0].param_types.len);
    try testing.expectEqual(ast.Type.Int, sigs[0].param_types[0].base);
    try testing.expectEqual(ast.Type.Int, sigs[0].param_types[1].base);
}

test "inline zig: accepts multiline signatures" {
    const src =
        \\fn foo(
        \\    a: i64,
        \\    b: f64,
        \\) f64 {
        \\    _ = a;
        \\    return b;
        \\}
    ;

    const sigs = try inline_zig.sanitizeAndExtract(testing.allocator, src);
    defer inline_zig.deinitSigs(testing.allocator, sigs);

    try testing.expectEqual(@as(usize, 1), sigs.len);
    try testing.expectEqualStrings("foo", sigs[0].name);
    try testing.expectEqual(ast.Type.Float, sigs[0].return_type.base);
    try testing.expectEqual(@as(usize, 2), sigs[0].param_types.len);
    try testing.expectEqual(ast.Type.Int, sigs[0].param_types[0].base);
    try testing.expectEqual(ast.Type.Float, sigs[0].param_types[1].base);
}

test "inline zig: rejects non-import const at top level" {
    const src = "const x = 123;";
    try testing.expectError(error.InlineZigNotValid, inline_zig.sanitizeAndExtract(testing.allocator, src));
}

test "inline zig: rejects invalid parameter type" {
    const src = "fn bad(x: i32) i64 { _ = x; return 0; }";
    try testing.expectError(error.InvalidParamType, inline_zig.sanitizeAndExtract(testing.allocator, src));
}

test "inline zig: rejects invalid return type" {
    const src = "fn bad(x: i64) i32 { _ = x; return 0; }";
    try testing.expectError(error.InvalidReturnType, inline_zig.sanitizeAndExtract(testing.allocator, src));
}

test "inline zig: rejects extern declarations without bodies" {
    const src = "extern fn f(x: i64) i64;";
    try testing.expectError(error.InlineZigNotValid, inline_zig.sanitizeAndExtract(testing.allocator, src));
}
