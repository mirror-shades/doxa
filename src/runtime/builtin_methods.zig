const std = @import("std");
const token = @import("../types/token.zig");
const ast = @import("../ast/ast.zig");

pub const BuiltinMethodInfo = struct {
    arg_count_min: usize,
    arg_count_max: ?usize,
    input_types: []const InputTypeSpec,
    return_type: ast.Type,
    can_panic: bool,
    name: []const u8,
};

pub const InputTypeSpec = union(enum) {
    Single: ast.Type,
    Union: []const ast.Type,
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
        .RANDOM => &METHODS[13],
        .OS => &METHODS[14],
        .ARCH => &METHODS[15],
        .ABI => &METHODS[16],
        .TIME => &METHODS[17],
        .TICK => &METHODS[18],
        .PRINT => &METHODS[19],
        .INPUT => &METHODS[20],
        .ASSERT => &METHODS[21],
        .PANIC => &METHODS[22],
        .EXIT => &METHODS[23],
        .SLEEP => &METHODS[24],
        .BUILD => &METHODS[25],
        .READ => &METHODS[26],
        .ARGC => &METHODS[27],
        .ARGV => &METHODS[28],
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

const METHODS = [_]BuiltinMethodInfo{
    .{
        .name = "length",
        .arg_count_min = 1,
        .arg_count_max = 1,
        .input_types = &[_]InputTypeSpec{Input{ .Union = &array_string }},
        .return_type = T.Int,
        .can_panic = false,
    },
    .{
        .name = "push",
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
        .arg_count_min = 1,
        .arg_count_max = 1,
        .input_types = &[_]InputTypeSpec{Input{ .Union = &array_string }},
        .return_type = T.Nothing,
        .can_panic = true,
    },
    .{
        .name = "insert",
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
        .arg_count_min = 1,
        .arg_count_max = 1,
        .input_types = &[_]InputTypeSpec{Input{ .Union = &array_string }},
        .return_type = T.Nothing,
        .can_panic = true,
    },
    .{
        .name = "find",
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
        .arg_count_min = 1,
        .arg_count_max = 1,
        .input_types = &[_]InputTypeSpec{Input{ .Any = {} }},
        .return_type = T.String,
        .can_panic = false,
    },
    .{
        .name = "int",
        .arg_count_min = 1,
        .arg_count_max = 1,
        .input_types = &[_]InputTypeSpec{Input{ .Union = &float_byte_string }},
        .return_type = T.Int,
        .can_panic = true,
    },
    .{
        .name = "float",
        .arg_count_min = 1,
        .arg_count_max = 1,
        .input_types = &[_]InputTypeSpec{Input{ .Union = &int_byte_string }},
        .return_type = T.Float,
        .can_panic = true,
    },
    .{
        .name = "byte",
        .arg_count_min = 1,
        .arg_count_max = 1,
        .input_types = &[_]InputTypeSpec{Input{ .Union = &int_float_string }},
        .return_type = T.Byte,
        .can_panic = true,
    },
    .{
        .name = "type",
        .arg_count_min = 1,
        .arg_count_max = 1,
        .input_types = &[_]InputTypeSpec{Input{ .Any = {} }},
        .return_type = T.String,
        .can_panic = false,
    },
    .{
        .name = "random",
        .arg_count_min = 0,
        .arg_count_max = 0,
        .input_types = &[_]InputTypeSpec{},
        .return_type = T.Float,
        .can_panic = false,
    },
    .{
        .name = "os",
        .arg_count_min = 0,
        .arg_count_max = 0,
        .input_types = &[_]InputTypeSpec{},
        .return_type = T.String,
        .can_panic = false,
    },
    .{
        .name = "arch",
        .arg_count_min = 0,
        .arg_count_max = 0,
        .input_types = &[_]InputTypeSpec{},
        .return_type = T.String,
        .can_panic = false,
    },
    .{
        .name = "abi",
        .arg_count_min = 0,
        .arg_count_max = 0,
        .input_types = &[_]InputTypeSpec{},
        .return_type = T.String,
        .can_panic = false,
    },
    .{
        .name = "time",
        .arg_count_min = 0,
        .arg_count_max = 0,
        .input_types = &[_]InputTypeSpec{},
        .return_type = T.Int,
        .can_panic = false,
    },
    .{
        .name = "tick",
        .arg_count_min = 0,
        .arg_count_max = 0,
        .input_types = &[_]InputTypeSpec{},
        .return_type = T.Int,
        .can_panic = false,
    },
    .{
        .name = "print",
        .arg_count_min = 1,
        .arg_count_max = null,
        .input_types = &[_]InputTypeSpec{Input{ .Single = T.String }},
        .return_type = T.Nothing,
        .can_panic = true,
    },
    .{
        .name = "input",
        .arg_count_min = 0,
        .arg_count_max = 0,
        .input_types = &[_]InputTypeSpec{},
        .return_type = T.String,
        .can_panic = true,
    },
    .{
        .name = "assert",
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
        .arg_count_min = 1,
        .arg_count_max = 1,
        .input_types = &[_]InputTypeSpec{Input{ .Single = T.String }},
        .return_type = T.Nothing,
        .can_panic = true,
    },
    .{
        .name = "exit",
        .arg_count_min = 1,
        .arg_count_max = 1,
        .input_types = &[_]InputTypeSpec{Input{ .Union = &int_byte }},
        .return_type = T.Nothing,
        .can_panic = true,
    },
    .{
        .name = "sleep",
        .arg_count_min = 1,
        .arg_count_max = 1,
        .input_types = &[_]InputTypeSpec{Input{ .Union = &int_byte }},
        .return_type = T.Nothing,
        .can_panic = true,
    },
    .{
        .name = "build",
        .arg_count_min = 6,
        .arg_count_max = 6,
        .input_types = &[_]InputTypeSpec{
            Input{ .Single = T.String }, // src
            Input{ .Single = T.String }, // out
            Input{ .Single = T.String }, // arch
            Input{ .Single = T.String }, // os
            Input{ .Single = T.String }, // abi
            Input{ .Single = T.Tetra }, // debug
        },
        .return_type = T.Int,
        .can_panic = false,
    },
    .{
        .name = "read",
        .arg_count_min = 1,
        .arg_count_max = 1,
        .input_types = &[_]InputTypeSpec{Input{ .Single = T.String }},
        .return_type = T.String,
        .can_panic = true,
    },
    .{
        .name = "argc",
        .arg_count_min = 0,
        .arg_count_max = 0,
        .input_types = &[_]InputTypeSpec{},
        .return_type = T.Int,
        .can_panic = false,
    },
    .{
        .name = "argv",
        .arg_count_min = 1,
        .arg_count_max = 1,
        .input_types = &[_]InputTypeSpec{Input{ .Single = T.Int }},
        .return_type = T.String,
        .can_panic = true,
    },
};
