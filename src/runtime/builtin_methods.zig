const std = @import("std");
const token = @import("../types/token.zig");
const ast = @import("../ast/ast.zig");

pub const BuiltinMethodInfo = struct {
    arg_count_min: usize,
    arg_count_max: ?usize,
    input_types: []const InputTypeSpec,
    return_type: ast.Type,
    /// Element type of an `.Array` return (`@unpack(...) -> byte[]`). The
    /// type system only has an untyped array; this names the element so the
    /// `string` ↔ `byte[]` barrier stays explicit at both ends.
    return_element_type: ?ast.Type = null,
    can_panic: bool,
    /// Whether the builtin writes through its subject argument in place
    /// (`@push`/`@pop`/`@insert`/`@remove`/`@clear`). Defaults to `true` so an
    /// unannotated builtin is treated as mutating; read-only intrinsics opt out
    /// (the read-only analysis in `param_mutation.zig` relies on this).
    mutates_arg: bool = true,
    name: []const u8,
    /// Display names for the parameters, positionally aligned with
    /// `input_types`. Purely presentational (editor signature help and
    /// snippets); the compiler never reads these. Builtins with no inputs
    /// leave this empty.
    param_names: []const []const u8 = &.{},
};

pub const InputTypeSpec = union(enum) {
    Single: ast.Type,
    Union: []const ast.Type,
    /// An array whose elements are of the given type (`@pack(byte[])`).
    TypedArray: ast.Type,
    Any,
    Integer,
    Collection,
};

pub fn getMethodInfo(method_type: token.TokenType) ?*const BuiltinMethodInfo {
    return switch (method_type) {
        .LENGTH => &METHODS[0],
        .PUSH => &METHODS[1],
        .POP => &METHODS[2],
        .INSERT => &METHODS[3],
        .REMOVE => &METHODS[4],
        .CLEAR => &METHODS[5],
        .FIND => &METHODS[6],
        .SLICE => &METHODS[7],
        .TOSTRING => &METHODS[8],
        .TOINT => &METHODS[9],
        .TOFLOAT => &METHODS[10],
        .TOBYTE => &METHODS[11],
        .TYPE => &METHODS[12],
        .PRINT => &METHODS[13],
        .ASSERT => &METHODS[14],
        .PANIC => &METHODS[15],
        .EXIT => &METHODS[16],
        .STD => &METHODS[17],
        .PACK => &METHODS[18],
        .UNPACK => &METHODS[19],
        else => null,
    };
}

pub fn getMethodInfoByName(name: []const u8) ?*const BuiltinMethodInfo {
    inline for (METHODS) |method| {
        if (std.mem.eql(u8, method.name, name)) {
            return &method;
        }
    }
    return null;
}

pub fn canMethodPanic(method_type: token.TokenType) bool {
    if (getMethodInfo(method_type)) |info| {
        return info.can_panic;
    }
    return false;
}

pub fn getArgCountRange(method_type: token.TokenType) ?struct { min: usize, max: usize } {
    if (getMethodInfo(method_type)) |info| {
        return .{
            .min = info.arg_count_min,
            .max = info.arg_count_max orelse info.arg_count_min,
        };
    }
    return null;
}

pub fn validateArgCount(method_type: token.TokenType, arg_count: usize) bool {
    if (getMethodInfo(method_type)) |info| {
        const max = info.arg_count_max orelse info.arg_count_min;
        return arg_count >= info.arg_count_min and arg_count <= max;
    }
    return false;
}

pub fn validateArgCountByName(name: []const u8, arg_count: usize) bool {
    if (getMethodInfoByName(name)) |info| {
        const max = info.arg_count_max orelse info.arg_count_min;
        return arg_count >= info.arg_count_min and arg_count <= max;
    }
    return false;
}

pub fn getArgCountRangeByName(name: []const u8) ?struct { min: usize, max: usize } {
    if (getMethodInfoByName(name)) |info| {
        return .{
            .min = info.arg_count_min,
            .max = info.arg_count_max orelse info.arg_count_min,
        };
    }
    return null;
}

const T = ast.Type;
const Input = InputTypeSpec;

const array_string = [_]ast.Type{ T.Array, T.String };
const int_byte = [_]ast.Type{ T.Int, T.Byte };
const float_byte_string = [_]ast.Type{ T.Float, T.Byte, T.String };
const int_byte_string = [_]ast.Type{ T.Int, T.Byte, T.String };
const int_float_string = [_]ast.Type{ T.Int, T.Float, T.String };

const value_names = [_][]const u8{"value"};
const collection_names = [_][]const u8{"collection"};
const collection_value_names = [_][]const u8{ "collection", "value" };
const collection_index_names = [_][]const u8{ "collection", "index" };
const collection_index_value_names = [_][]const u8{ "collection", "index", "value" };
const collection_start_length_names = [_][]const u8{ "collection", "start", "length" };
const format_names = [_][]const u8{"format"};
const condition_message_names = [_][]const u8{ "condition", "message" };
const message_names = [_][]const u8{"message"};
const code_names = [_][]const u8{"code"};
const bytes_names = [_][]const u8{"bytes"};
const word_names = [_][]const u8{"word"};

const METHODS = [_]BuiltinMethodInfo{
    .{
        .name = "length",
        .param_names = &value_names,
        .mutates_arg = false,
        .arg_count_min = 1,
        .arg_count_max = 1,
        .input_types = &[_]InputTypeSpec{Input{ .Union = &array_string }},
        .return_type = T.Int,
        .can_panic = false,
    },
    .{
        .name = "push",
        .param_names = &collection_value_names,
        .arg_count_min = 2,
        .arg_count_max = 2,
        .input_types = &[_]InputTypeSpec{
            Input{ .Union = &array_string },
            Input{ .Any = {} },
        },
        .return_type = T.Nothing,
        .can_panic = true,
    },
    .{
        .name = "pop",
        .param_names = &collection_names,
        .arg_count_min = 1,
        .arg_count_max = 1,
        .input_types = &[_]InputTypeSpec{Input{ .Union = &array_string }},
        .return_type = T.Nothing,
        .can_panic = true,
    },
    .{
        .name = "insert",
        .param_names = &collection_index_value_names,
        .arg_count_min = 3,
        .arg_count_max = 3,
        .input_types = &[_]InputTypeSpec{
            Input{ .Union = &array_string },
            Input{ .Single = T.Int },
            Input{ .Any = {} },
        },
        .return_type = T.Nothing,
        .can_panic = true,
    },
    .{
        .name = "remove",
        .param_names = &collection_index_names,
        .arg_count_min = 2,
        .arg_count_max = 2,
        .input_types = &[_]InputTypeSpec{
            Input{ .Union = &array_string },
            Input{ .Single = T.Int },
        },
        .return_type = T.Nothing,
        .can_panic = true,
    },
    .{
        .name = "clear",
        .param_names = &collection_names,
        .arg_count_min = 1,
        .arg_count_max = 1,
        .input_types = &[_]InputTypeSpec{Input{ .Union = &array_string }},
        .return_type = T.Nothing,
        .can_panic = true,
    },
    .{
        .name = "find",
        .param_names = &collection_value_names,
        .mutates_arg = false,
        .arg_count_min = 2,
        .arg_count_max = 2,
        .input_types = &[_]InputTypeSpec{
            Input{ .Union = &array_string },
            Input{ .Any = {} },
        },
        .return_type = T.Int,
        .can_panic = true,
    },
    .{
        .name = "slice",
        .param_names = &collection_start_length_names,
        .mutates_arg = false,
        .arg_count_min = 3,
        .arg_count_max = 3,
        .input_types = &[_]InputTypeSpec{
            Input{ .Union = &array_string },
            Input{ .Single = T.Int },
            Input{ .Single = T.Int },
        },
        .return_type = T.Nothing,
        .can_panic = true,
    },
    .{
        .name = "string",
        .param_names = &value_names,
        .mutates_arg = false,
        .arg_count_min = 1,
        .arg_count_max = 1,
        .input_types = &[_]InputTypeSpec{Input{ .Any = {} }},
        .return_type = T.String,
        .can_panic = false,
    },
    .{
        .name = "int",
        .param_names = &value_names,
        .mutates_arg = false,
        .arg_count_min = 1,
        .arg_count_max = 1,
        .input_types = &[_]InputTypeSpec{Input{ .Union = &float_byte_string }},
        .return_type = T.Int,
        .can_panic = true,
    },
    .{
        .name = "float",
        .param_names = &value_names,
        .mutates_arg = false,
        .arg_count_min = 1,
        .arg_count_max = 1,
        .input_types = &[_]InputTypeSpec{Input{ .Union = &int_byte_string }},
        .return_type = T.Float,
        .can_panic = true,
    },
    .{
        .name = "byte",
        .param_names = &value_names,
        .mutates_arg = false,
        .arg_count_min = 1,
        .arg_count_max = 1,
        .input_types = &[_]InputTypeSpec{Input{ .Union = &int_float_string }},
        .return_type = T.Byte,
        .can_panic = true,
    },
    .{
        .name = "type",
        .param_names = &value_names,
        .mutates_arg = false,
        .arg_count_min = 1,
        .arg_count_max = 1,
        .input_types = &[_]InputTypeSpec{Input{ .Any = {} }},
        .return_type = T.String,
        .can_panic = false,
    },
    .{
        .name = "print",
        .param_names = &format_names,
        .mutates_arg = false,
        .arg_count_min = 1,
        .arg_count_max = null,
        .input_types = &[_]InputTypeSpec{Input{ .Single = T.String }},
        .return_type = T.Nothing,
        .can_panic = true,
    },
    .{
        .name = "assert",
        .param_names = &condition_message_names,
        .mutates_arg = false,
        .arg_count_min = 1,
        .arg_count_max = 2,
        .input_types = &[_]InputTypeSpec{
            Input{ .Single = T.Tetra },
            Input{ .Single = T.String },
        },
        .return_type = T.Nothing,
        .can_panic = true,
    },
    .{
        .name = "panic",
        .param_names = &message_names,
        .mutates_arg = false,
        .arg_count_min = 1,
        .arg_count_max = 1,
        .input_types = &[_]InputTypeSpec{Input{ .Single = T.String }},
        .return_type = T.Nothing,
        .can_panic = true,
    },
    .{
        .name = "exit",
        .param_names = &code_names,
        .mutates_arg = false,
        .arg_count_min = 1,
        .arg_count_max = 1,
        .input_types = &[_]InputTypeSpec{Input{ .Union = &int_byte }},
        .return_type = T.Nothing,
        .can_panic = true,
    },
    .{
        .name = "std",
        .mutates_arg = false,
        .arg_count_min = 0,
        .arg_count_max = 0,
        .input_types = &[_]InputTypeSpec{},
        .return_type = T.String,
        .can_panic = false,
    },
    .{
        .name = "pack",
        .param_names = &bytes_names,
        .mutates_arg = false,
        .arg_count_min = 1,
        .arg_count_max = 1,
        .input_types = &[_]InputTypeSpec{Input{ .TypedArray = T.Byte }},
        .return_type = T.String,
        .can_panic = true,
    },
    .{
        .name = "unpack",
        .param_names = &word_names,
        .mutates_arg = false,
        .arg_count_min = 1,
        .arg_count_max = 1,
        .input_types = &[_]InputTypeSpec{Input{ .Single = T.String }},
        .return_type = T.Array,
        .return_element_type = T.Byte,
        .can_panic = false,
    },
};

/// Every builtin in declaration order. The single source of truth shared by
/// the compiler and the language server's completion/signature help.
pub fn all() []const BuiltinMethodInfo {
    return &METHODS;
}
