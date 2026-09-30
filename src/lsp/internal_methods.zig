//! Editor-facing presentation for the compiler's `@`-prefixed builtins.
//!
//! The signatures, arity, and parameter names all come from the canonical
//! `builtin_methods.zig` table — the same one the compiler checks calls
//! against — so completion, signature help, and the compiler can never
//! disagree about what a builtin accepts. This module only adds the prose
//! descriptions and renders the canonical data into editor text.

const std = @import("std");
const builtins = @import("../runtime/builtin_methods.zig");
const ast = @import("../ast/ast.zig");

/// Prose for a builtin, keyed by its bare name (no `@`).
const Doc = struct {
    name: []const u8,
    text: []const u8,
    /// Return text for builtins whose result depends on the receiver, which
    /// the canonical table cannot express (it uses `nothing` as a sentinel).
    return_display: ?[]const u8 = null,
};

const DOCS = [_]Doc{
    .{ .name = "length", .text = "Return the number of elements in a collection, or the byte length of a string." },
    .{ .name = "push", .text = "Append `value` to the end of an array, or concatenate it onto a string." },
    .{ .name = "pop", .text = "Remove and return the last element. Panics on an empty collection.", .return_display = "string | element" },
    .{ .name = "insert", .text = "Insert `value` at `index`. Panics on an out-of-range index." },
    .{ .name = "remove", .text = "Remove and return the element at `index`. Panics on an out-of-range index.", .return_display = "string | element" },
    .{ .name = "clear", .text = "Remove every element, leaving the collection empty." },
    .{ .name = "find", .text = "Return the first index of `value`, or `-1` when it is missing." },
    .{ .name = "slice", .text = "Return the sub-collection from `start` for `length` elements. Panics on an invalid range.", .return_display = "string | array" },
    .{ .name = "string", .text = "Convert any value to its human-readable string form." },
    .{ .name = "int", .text = "Convert a `float`, `byte`, or `string` to `int`. Panics on invalid format or overflow." },
    .{ .name = "float", .text = "Convert an `int`, `byte`, or `string` to `float`. Panics on invalid format." },
    .{ .name = "byte", .text = "Convert an `int`, `float`, or `string` to `byte`. Panics on overflow or invalid format." },
    .{ .name = "type", .text = "Return the runtime type name of a value as a string." },
    .{ .name = "print", .text = "Write formatted text to standard output. Panics on I/O failure." },
    .{ .name = "assert", .text = "Panic with `message` when `condition` is not true." },
    .{ .name = "panic", .text = "Halt the program with `message`." },
    .{ .name = "exit", .text = "Terminate the program with status `code`." },
    .{ .name = "std", .text = "Return the filesystem path to the standard-library root." },
    .{ .name = "pack", .text = "Pack a `byte[]` into a `string`: each byte becomes a u8 codepoint." },
    .{ .name = "unpack", .text = "Unpack a `string` into its `byte[]` representation." },
};

/// Every builtin, in canonical declaration order.
pub fn all() []const builtins.BuiltinMethodInfo {
    return builtins.all();
}

/// Resolves a label with or without the leading `@` to its canonical entry.
pub fn find(label: []const u8) ?*const builtins.BuiltinMethodInfo {
    const name = if (label.len > 0 and label[0] == '@') label[1..] else label;
    return builtins.getMethodInfoByName(name);
}

/// The prose description for a builtin, or `""` when none is registered.
pub fn documentation(info: *const builtins.BuiltinMethodInfo) []const u8 {
    for (DOCS) |doc| {
        if (std.mem.eql(u8, doc.name, info.name)) return doc.text;
    }
    return "";
}

fn returnDisplay(info: *const builtins.BuiltinMethodInfo) ?[]const u8 {
    for (DOCS) |doc| {
        if (std.mem.eql(u8, doc.name, info.name)) return doc.return_display;
    }
    return null;
}

fn typeLabel(t: ast.Type) []const u8 {
    return switch (t) {
        .Int => "int",
        .Byte => "byte",
        .Float => "float",
        .String => "string",
        .Tetra => "tetra",
        .Array => "array",
        .Function => "function",
        .Struct => "struct",
        .Enum => "enum",
        .Custom => "custom",
        .Map => "map",
        .Nothing => "nothing",
        .Union => "union",
    };
}

fn writeInputSpec(writer: *std.Io.Writer, spec: builtins.InputTypeSpec) !void {
    switch (spec) {
        .Single => |t| try writer.writeAll(typeLabel(t)),
        .Union => |types| {
            for (types, 0..) |t, i| {
                if (i > 0) try writer.writeAll(" | ");
                try writer.writeAll(typeLabel(t));
            }
        },
        .TypedArray => |t| {
            try writer.writeAll(typeLabel(t));
            try writer.writeAll("[]");
        },
        .Any => try writer.writeAll("any"),
        .Integer => try writer.writeAll("int"),
        .Collection => try writer.writeAll("array | string"),
    }
}

fn writeReturn(writer: *std.Io.Writer, info: *const builtins.BuiltinMethodInfo) !void {
    if (returnDisplay(info)) |override| {
        try writer.writeAll(override);
        return;
    }
    if (info.return_type == .Array) {
        if (info.return_element_type) |element| {
            try writer.print("{s}[]", .{typeLabel(element)});
            return;
        }
    }
    try writer.writeAll(typeLabel(info.return_type));
}

/// A one-line human signature: `@insert(collection: array | string, index: int, value: any) -> nothing`.
pub fn signature(allocator: std.mem.Allocator, info: *const builtins.BuiltinMethodInfo) ![]u8 {
    var out = std.Io.Writer.Allocating.init(allocator);
    errdefer out.deinit();
    const w = &out.writer;

    try w.print("@{s}(", .{info.name});
    for (info.input_types, 0..) |spec, i| {
        if (i > 0) try w.writeAll(", ");
        const optional = i >= info.arg_count_min;
        if (optional) try w.writeAll("[");
        try writeParameter(w, info, i, spec);
        if (optional) try w.writeAll("]");
    }
    if (info.arg_count_max == null) {
        if (info.input_types.len > 0) try w.writeAll(", ");
        try w.writeAll("...");
    }
    try w.writeAll(") -> ");
    try writeReturn(w, info);
    return out.toOwnedSlice();
}

/// A single parameter rendered as `name: type` (or just the type when the
/// name is unregistered).
pub fn parameterLabel(
    allocator: std.mem.Allocator,
    info: *const builtins.BuiltinMethodInfo,
    index: usize,
) ![]u8 {
    var out = std.Io.Writer.Allocating.init(allocator);
    errdefer out.deinit();
    const spec = if (index < info.input_types.len) info.input_types[index] else null;
    if (spec) |s| {
        try writeParameter(&out.writer, info, index, s);
    }
    return out.toOwnedSlice();
}

fn writeParameter(
    writer: *std.Io.Writer,
    info: *const builtins.BuiltinMethodInfo,
    index: usize,
    spec: builtins.InputTypeSpec,
) !void {
    if (index < info.param_names.len and info.param_names[index].len > 0) {
        try writer.print("{s}: ", .{info.param_names[index]});
    }
    try writeInputSpec(writer, spec);
}

/// A snippet body that inserts the builtin and a tab stop per argument:
/// `@insert($1, $2, $3)`. Purely a convenience; the placeholder count is the
/// declared argument count (the minimum when the builtin is variadic).
pub fn snippet(allocator: std.mem.Allocator, info: *const builtins.BuiltinMethodInfo) ![]u8 {
    var out = std.Io.Writer.Allocating.init(allocator);
    errdefer out.deinit();
    const w = &out.writer;

    try w.print("@{s}(", .{info.name});
    const placeholders = info.arg_count_max orelse info.arg_count_min;
    var i: usize = 0;
    while (i < placeholders) : (i += 1) {
        if (i > 0) try w.writeAll(", ");
        try w.print("${d}", .{i + 1});
    }
    try w.writeAll(")");
    return out.toOwnedSlice();
}

test "every canonical builtin has editor prose" {
    for (all()) |*info| {
        try std.testing.expect(documentation(info).len > 0);
    }
    try std.testing.expectEqual(DOCS.len, all().len);
}

test "builtin signatures render arity, optional params, and variadics" {
    const allocator = std.testing.allocator;

    const insert = find("@insert") orelse return error.MissingBuiltin;
    const insert_sig = try signature(allocator, insert);
    defer allocator.free(insert_sig);
    try std.testing.expectEqualStrings(
        "@insert(collection: array | string, index: int, value: any) -> nothing",
        insert_sig,
    );

    const assert_sig = try signature(allocator, find("assert").?);
    defer allocator.free(assert_sig);
    try std.testing.expectEqualStrings("@assert(condition: tetra, [message: string]) -> nothing", assert_sig);

    const print_sig = try signature(allocator, find("print").?);
    defer allocator.free(print_sig);
    try std.testing.expectEqualStrings("@print(format: string, ...) -> nothing", print_sig);

    const pop_sig = try signature(allocator, find("pop").?);
    defer allocator.free(pop_sig);
    try std.testing.expectEqualStrings("@pop(collection: array | string) -> string | element", pop_sig);

    const unpack_sig = try signature(allocator, find("unpack").?);
    defer allocator.free(unpack_sig);
    try std.testing.expectEqualStrings("@unpack(word: string) -> byte[]", unpack_sig);
}

test "builtin snippets insert one placeholder per argument" {
    const allocator = std.testing.allocator;

    const insert = try snippet(allocator, find("insert").?);
    defer allocator.free(insert);
    try std.testing.expectEqualStrings("@insert($1, $2, $3)", insert);

    const assert_snip = try snippet(allocator, find("assert").?);
    defer allocator.free(assert_snip);
    try std.testing.expectEqualStrings("@assert($1, $2)", assert_snip);

    const print_snip = try snippet(allocator, find("print").?);
    defer allocator.free(print_snip);
    try std.testing.expectEqualStrings("@print($1)", print_snip);

    const std_snip = try snippet(allocator, find("@std").?);
    defer allocator.free(std_snip);
    try std.testing.expectEqualStrings("@std()", std_snip);
}
