const std = @import("std");

/// VM OpCode enumeration - matches the opcodes used in your VM
pub const OpCode = enum(u8) {
    // Core execution
    OP_HALT,
    OP_CONST,
    OP_VAR,
    OP_SET_VAR,
    OP_SET_CONST,

    // Assertion handling
    OP_ASSERT_FAIL,

    // Arithmetic operations
    OP_IADD,
    OP_ISUB,
    OP_IMUL,
    OP_FADD,
    OP_FSUB,
    OP_FMUL,
    OP_FDIV,

    // Comparison operations
    OP_EQUAL,
    OP_NOTEQUAL,
    OP_GREATER,
    OP_LESS,
    OP_CONVERT_NUMBER,

    // Control flow
    OP_JUMP,
    OP_JUMP_IF,
    OP_JUMP_IF_FALSE,
    OP_RETURN,
    OP_CALL,

    // String operations
    OP_STR_CONCAT,
    OP_STR_EQ,
    OP_STR_LEN,
    OP_SUBSTR,

    // Struct operations
    OP_STRUCT_NEW,
    OP_SET_FIELD,
    OP_GET_FIELD,

    // Array operations
    OP_ARRAY_NEW,
    OP_ARRAY_PUSH,
    OP_ARRAY_LEN,
    OP_ARRAY_GET,
    OP_ARRAY_SET,
    OP_ARRAY_CONCAT,
    OP_ARRAY_SLICE,

    // Stack operations
    OP_DUP,
    OP_POP,

    // Block/scope operations
    OP_BEGIN_BLOCK,
    OP_END_BLOCK,

    // Logical operations
    OP_AND,
    OP_OR,
    OP_NOT,

    // Pattern matching
    OP_MATCH,
    OP_MATCH_ARM,
    OP_MATCH_END,

    // Type operations
    OP_IS_NOTHING,
    OP_TYPEOF,

    // Exception handling
    OP_TRY,
    OP_CATCH,
    OP_THROW,
    OP_END_TRY,
};

/// Value types for the VM
pub const ValueType = enum {
    INT,
    FLOAT,
    STRING,
    BOOL,
    ARRAY,
    STRUCT,
    FUNCTION,
    NOTHING,
};

/// Union for different value data types
pub const ValueData = union(ValueType) {
    INT: void,
    FLOAT: void,
    STRING: void,
    BOOL: void,
    ARRAY: void,
    STRUCT: void,
    FUNCTION: void,
    NOTHING: void,
};

/// VM Value representation
pub const Value = struct {
    type: ValueType,
    nothing: bool,
    data: union {
        int: i32,
        float: f64,
        string: []const u8,
        boolean: bool,
        // Add other data types as needed
    },

    /// Create an integer value
    pub fn initInt(value: i32) Value {
        return Value{
            .type = .INT,
            .nothing = false,
            .data = .{ .int = value },
        };
    }

    /// Create a float value
    pub fn initFloat(value: f64) Value {
        return Value{
            .type = .FLOAT,
            .nothing = false,
            .data = .{ .float = value },
        };
    }

    /// Create a string value
    pub fn initString(value: []const u8) Value {
        return Value{
            .type = .STRING,
            .nothing = false,
            .data = .{ .string = value },
        };
    }

    /// Create a boolean value
    pub fn initBool(value: bool) Value {
        return Value{
            .type = .BOOL,
            .nothing = false,
            .data = .{ .boolean = value },
        };
    }

    /// Create a nothing value
    pub fn initNothing() Value {
        return Value{
            .type = .NOTHING,
            .nothing = true,
            .data = .{ .int = 0 }, // Unused
        };
    }

    /// Extract integer value
    pub fn asInt(self: Value) !i32 {
        if (self.type != .INT) return error.TypeError;
        return self.data.int;
    }

    /// Extract float value
    pub fn asFloat(self: Value) !f64 {
        if (self.type != .FLOAT) return error.TypeError;
        return self.data.float;
    }

    /// Extract string value
    pub fn asString(self: Value) ![]const u8 {
        if (self.type != .STRING) return error.TypeError;
        return self.data.string;
    }

    /// Extract boolean value
    pub fn asBool(self: Value) !bool {
        if (self.type != .BOOL) return error.TypeError;
        return self.data.boolean;
    }

    /// Check if value is truthy
    pub fn isTruthy(self: Value) bool {
        if (self.nothing) return false;

        return switch (self.type) {
            .BOOL => self.data.boolean,
            .INT => self.data.int != 0,
            .FLOAT => self.data.float != 0.0,
            .STRING => self.data.string.len > 0,
            .NOTHING => false,
            else => true, // Arrays, structs, functions are truthy if not nothing
        };
    }

    /// Check if two values are equal
    pub fn equals(self: Value, other: Value) bool {
        if (self.nothing != other.nothing) return false;
        if (self.nothing and other.nothing) return true;
        if (self.type != other.type) return false;

        return switch (self.type) {
            .INT => self.data.int == other.data.int,
            .FLOAT => self.data.float == other.data.float,
            .BOOL => self.data.boolean == other.data.boolean,
            .STRING => std.mem.eql(u8, self.data.string, other.data.string),
            .NOTHING => true,
            else => false, // TODO: Add equality for complex types
        };
    }
};
