const std = @import("std");
const token = @import("../types/token.zig");
const ast = @import("../ast/ast.zig");

/// Represents the metadata for a built-in method
pub const BuiltinMethodInfo = struct {
    /// Minimum number of arguments required
    arg_count_min: usize,
    /// Maximum number of arguments allowed (null means same as min)
    arg_count_max: ?usize,
    /// Expected input types for each argument position
    /// For flexible types like "array | string", use a special marker
    input_types: []const InputTypeSpec,
    /// Return type of the method
    return_type: ast.Type,
    /// Whether this method can panic/error
    can_panic: bool,
    /// Method name (for error messages)
    name: []const u8,
};

/// Specifies the expected type(s) for an argument
/// Some methods accept multiple types (e.g., array | string)
pub const InputTypeSpec = union(enum) {
    /// Single specific type
    Single: ast.Type,
    /// Multiple possible types (e.g., array | string)
    Union: []const ast.Type,
    /// Any type (for methods like @string, @type)
    Any,
    /// Integer types (int | byte)
    Integer,
    /// Collection types (array | string)
    Collection,
};

/// Get the metadata for a built-in method by its TokenType
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
        else => null,
    };
}

/// Get method info by name (for BuiltinCall expressions that use string names)
pub fn getMethodInfoByName(name: []const u8) ?*const BuiltinMethodInfo {
    inline for (METHODS) |method| {
        if (std.mem.eql(u8, method.name, name)) {
            return &method;
        }
    }
    return null;
}

/// Check if a method can panic/error
pub fn canMethodPanic(method_type: token.TokenType) bool {
    if (getMethodInfo(method_type)) |info| {
        return info.can_panic;
    }
    return false;
}

/// Get the expected argument count range for a method
pub fn getArgCountRange(method_type: token.TokenType) ?struct { min: usize, max: usize } {
    if (getMethodInfo(method_type)) |info| {
        return .{
            .min = info.arg_count_min,
            .max = info.arg_count_max orelse info.arg_count_min,
        };
    }
    return null;
}

/// Check if the given argument count is valid for a method
pub fn validateArgCount(method_type: token.TokenType, arg_count: usize) bool {
    if (getMethodInfo(method_type)) |info| {
        const max = info.arg_count_max orelse info.arg_count_min;
        return arg_count >= info.arg_count_min and arg_count <= max;
    }
    return false;
}

/// Check if the given argument count is valid for a method by name
pub fn validateArgCountByName(name: []const u8, arg_count: usize) bool {
    if (getMethodInfoByName(name)) |info| {
        const max = info.arg_count_max orelse info.arg_count_min;
        return arg_count >= info.arg_count_min and arg_count <= max;
    }
    return false;
}

/// Get expected argument count range by name
pub fn getArgCountRangeByName(name: []const u8) ?struct { min: usize, max: usize } {
    if (getMethodInfoByName(name)) |info| {
        return .{
            .min = info.arg_count_min,
            .max = info.arg_count_max orelse info.arg_count_min,
        };
    }
    return null;
}

// Type constants for easier reference
const T = ast.Type;
const Input = InputTypeSpec;

// Helper arrays for union types
const array_string = [_]ast.Type{ T.Array, T.String };
const int_byte = [_]ast.Type{ T.Int, T.Byte };
const float_byte_string = [_]ast.Type{ T.Float, T.Byte, T.String };
const int_byte_string = [_]ast.Type{ T.Int, T.Byte, T.String };
const int_float_string = [_]ast.Type{ T.Int, T.Float, T.String };

/// All built-in method metadata
const METHODS = [_]BuiltinMethodInfo{
    // @length - array | string -> int
    .{
        .name = "length",
        .arg_count_min = 1,
        .arg_count_max = 1,
        .input_types = &[_]InputTypeSpec{Input{ .Union = &array_string }},
        .return_type = T.Int,
        .can_panic = false,
    },
    // @push - array | string, value -> nothing
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
    // @pop - array | string -> any
    .{
        .name = "pop",
        .arg_count_min = 1,
        .arg_count_max = 1,
        .input_types = &[_]InputTypeSpec{Input{ .Union = &array_string }},
        .return_type = T.Nothing, // Returns element type, but we use Nothing as base
        .can_panic = true,
    },
    // @insert - array | string, int, value -> nothing
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
    // @remove - array | string, int -> any
    .{
        .name = "remove",
        .arg_count_min = 2,
        .arg_count_max = 2,
        .input_types = &[_]InputTypeSpec{
            Input{ .Union = &array_string },
            Input{ .Single = T.Int },
        },
        .return_type = T.Nothing, // Returns element type
        .can_panic = true,
    },
    // @clear - array | string -> nothing
    .{
        .name = "clear",
        .arg_count_min = 1,
        .arg_count_max = 1,
        .input_types = &[_]InputTypeSpec{Input{ .Union = &array_string }},
        .return_type = T.Nothing,
        .can_panic = true,
    },
    // @find - array | string, value -> int
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
    // @slice - array | string, int, int -> array | string
    .{
        .name = "slice",
        .arg_count_min = 3,
        .arg_count_max = 3,
        .input_types = &[_]InputTypeSpec{
            Input{ .Union = &array_string },
            Input{ .Single = T.Int },
            Input{ .Single = T.Int },
        },
        .return_type = T.Nothing, // Returns same type as input
        .can_panic = true,
    },
    // @string - any -> string
    .{
        .name = "string",
        .arg_count_min = 1,
        .arg_count_max = 1,
        .input_types = &[_]InputTypeSpec{Input{ .Any = {} }},
        .return_type = T.String,
        .can_panic = false,
    },
    // @int - float | byte | string -> int
    .{
        .name = "int",
        .arg_count_min = 1,
        .arg_count_max = 1,
        .input_types = &[_]InputTypeSpec{Input{ .Union = &float_byte_string }},
        .return_type = T.Int,
        .can_panic = true,
    },
    // @float - int | byte | string -> float
    .{
        .name = "float",
        .arg_count_min = 1,
        .arg_count_max = 1,
        .input_types = &[_]InputTypeSpec{Input{ .Union = &int_byte_string }},
        .return_type = T.Float,
        .can_panic = true,
    },
    // @byte - int | float | string -> byte
    .{
        .name = "byte",
        .arg_count_min = 1,
        .arg_count_max = 1,
        .input_types = &[_]InputTypeSpec{Input{ .Union = &int_float_string }},
        .return_type = T.Byte,
        .can_panic = true,
    },
    // @type - any -> string
    .{
        .name = "type",
        .arg_count_min = 1,
        .arg_count_max = 1,
        .input_types = &[_]InputTypeSpec{Input{ .Any = {} }},
        .return_type = T.String,
        .can_panic = false,
    },
    // @random - none -> float
    .{
        .name = "random",
        .arg_count_min = 0,
        .arg_count_max = 0,
        .input_types = &[_]InputTypeSpec{},
        .return_type = T.Float,
        .can_panic = false,
    },
    // @os - none -> string
    .{
        .name = "os",
        .arg_count_min = 0,
        .arg_count_max = 0,
        .input_types = &[_]InputTypeSpec{},
        .return_type = T.String,
        .can_panic = false,
    },
    // @arch - none -> string
    .{
        .name = "arch",
        .arg_count_min = 0,
        .arg_count_max = 0,
        .input_types = &[_]InputTypeSpec{},
        .return_type = T.String,
        .can_panic = false,
    },
    // @abi - none -> string
    .{
        .name = "abi",
        .arg_count_min = 0,
        .arg_count_max = 0,
        .input_types = &[_]InputTypeSpec{},
        .return_type = T.String,
        .can_panic = false,
    },
    // @time - none -> int
    .{
        .name = "time",
        .arg_count_min = 0,
        .arg_count_max = 0,
        .input_types = &[_]InputTypeSpec{},
        .return_type = T.Int,
        .can_panic = false,
    },
    // @tick - none -> int
    .{
        .name = "tick",
        .arg_count_min = 0,
        .arg_count_max = 0,
        .input_types = &[_]InputTypeSpec{},
        .return_type = T.Int,
        .can_panic = false,
    },
    // @print - string -> nothing
    .{
        .name = "print",
        .arg_count_min = 1,
        .arg_count_max = null, // Variable args for string interpolation
        .input_types = &[_]InputTypeSpec{Input{ .Single = T.String }},
        .return_type = T.Nothing,
        .can_panic = true,
    },
    // @input - none -> string
    .{
        .name = "input",
        .arg_count_min = 0,
        .arg_count_max = 0,
        .input_types = &[_]InputTypeSpec{},
        .return_type = T.String,
        .can_panic = true,
    },
    // @assert - tetra, string? -> nothing
    .{
        .name = "assert",
        .arg_count_min = 1,
        .arg_count_max = 2,
        .input_types = &[_]InputTypeSpec{
            Input{ .Single = T.Tetra },
            Input{ .Single = T.String }, // Optional second arg
        },
        .return_type = T.Nothing,
        .can_panic = true,
    },
    // @panic - string -> never
    .{
        .name = "panic",
        .arg_count_min = 1,
        .arg_count_max = 1,
        .input_types = &[_]InputTypeSpec{Input{ .Single = T.String }},
        .return_type = T.Nothing, // never type, but we use Nothing
        .can_panic = true, // Always panics
    },
    // @exit - int? -> never
    .{
        .name = "exit",
        .arg_count_min = 1,
        .arg_count_max = 1,
        .input_types = &[_]InputTypeSpec{Input{ .Union = &int_byte }},
        .return_type = T.Nothing, // never type
        .can_panic = true, // Always terminates
    },
    // @sleep - int -> nothing
    .{
        .name = "sleep",
        .arg_count_min = 1,
        .arg_count_max = 1,
        .input_types = &[_]InputTypeSpec{Input{ .Union = &int_byte }},
        .return_type = T.Nothing,
        .can_panic = true,
    },
    // @build - string, string, string, string, string, tetra -> int
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
        .can_panic = false, // Returns exit code, doesn't panic
    },
    // @read - string -> string
    .{
        .name = "read",
        .arg_count_min = 1,
        .arg_count_max = 1,
        .input_types = &[_]InputTypeSpec{Input{ .Single = T.String }},
        .return_type = T.String,
        .can_panic = true, // Can panic if file not found or can't be read
    },
};
