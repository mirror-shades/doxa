const std = @import("std");

pub const OpCode = enum(u8) {
    OP_CONST,
    OP_IADD,
    OP_ISUB,
    OP_IMUL,
    OP_FDIV,
    OP_NEGATE,
    OP_NOT,
    OP_EQUAL,
    OP_NOTEQUAL,
    OP_GREATER,
    OP_LESS,
    OP_JUMP,
    OP_JUMP_IF_FALSE,
    OP_CALL,
    OP_RETURN,
    OP_POP,
    OP_GET_LOCAL,
    OP_SET_LOCAL,
    OP_GET_GLOBAL,
    OP_SET_GLOBAL,
    OP_GET_UPVALUE,
    OP_SET_UPVALUE,
    OP_GET_PROPERTY,
    OP_SET_PROPERTY,
    OP_GET_SUPER,
    OP_GET_INDEX,
    OP_SET_INDEX,
    OP_BUILD_LIST,
    OP_BUILD_MAP,
    OP_BUILD_STRUCT,
    OP_APPEND,
    OP_SLICE,
    OP_TRY,
    OP_CATCH,
    OP_END_TRY,
    OP_THROW,
    OP_SET_VAR,
    OP_SET_CONST,
    OP_HALT,
};

pub const ValueType = enum {
    Nothing,
    Number,
    Boolean,
    String,
    Function,
    NativeFunction,
    List,
    Map,
    Struct,
    Error,
};

pub const Value = union(ValueType) {
    Nothing: void,
    Number: f64,
    Boolean: bool,
    String: []const u8,
    Function: *Function,
    NativeFunction: NativeFunction,
    List: std.ArrayList(Value),
    Map: std.StringHashMap(Value),
    Struct: Struct,
    Error: []const u8,

    pub fn format(
        self: Value,
        comptime fmt: []const u8,
        options: std.fmt.FormatOptions,
        writer: anytype,
    ) !void {
        _ = fmt;
        _ = options;

        switch (self) {
            .Nothing => try writer.writeAll("nothing"),
            .Number => |n| try std.fmt.format(writer, "{d}", .{n}),
            .Boolean => |b| try writer.writeAll(if (b) "true" else "false"),
            .String => |s| try std.fmt.format(writer, "\"{s}\"", .{s}),
            .Function => |f| try std.fmt.format(writer, "<fn {s}>", .{f.name}),
            .NativeFunction => |native_function| {
                try std.fmt.format(writer, "<native fn {s}>", .{native_function.name});
            },
            .List => |l| {
                try writer.writeAll("[");
                for (l.items, 0..) |item, i| {
                    if (i > 0) try writer.writeAll(", ");
                    try item.format("", .{}, writer);
                }
                try writer.writeAll("]");
            },
            .Map => |m| {
                try writer.writeAll("{");
                var it = m.iterator();
                var first = true;
                while (it.next()) |entry| {
                    if (!first) try writer.writeAll(", ");
                    try std.fmt.format(writer, "\"{s}\": ", .{entry.key_ptr.*});
                    try entry.value_ptr.*.format("", .{}, writer);
                    first = false;
                }
                try writer.writeAll("}");
            },
            .Struct => |s| {
                try writer.writeAll(s.name);
                try writer.writeAll("{");
                var it = s.fields.iterator();
                var first = true;
                while (it.next()) |entry| {
                    if (!first) try writer.writeAll(", ");
                    try std.fmt.format(writer, ".{s} = ", .{entry.key_ptr.*});
                    try entry.value_ptr.*.format("", .{}, writer);
                    first = false;
                }
                try writer.writeAll("}");
            },
            .Error => |e| try std.fmt.format(writer, "error: {s}", .{e}),
        }
    }
};

pub const Function = struct {
    name: []const u8,
    arity: u8,
    code: []u8,
    constants: []Value,
};

pub const NativeFunction = struct {
    name: []const u8,
    arity: u8,
    function: *const fn ([]Value) Value,
};

pub const Variable = struct {
    index: usize,
    is_constant: bool,
    is_dynamic: bool,
    name: []const u8,
};

pub const Local = struct {
    name: []const u8,
    depth: i32,
    is_captured: bool,
};

pub const Struct = struct {
    name: []const u8,
    fields: std.StringHashMap(Value),
};
