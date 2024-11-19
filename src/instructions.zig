const std = @import("std");
const Reporting = @import("reporting.zig").Reporting;

pub const OpCode = enum(u8) {
    // Halt the program.
    OP_HALT = 0x00,

    // push a constant value into the stack.
    OP_CONST = 0x01,
    // push a variable value into the stack.
    OP_VAR = 0x02,
    // set a variable to a constant value.
    OP_SET_VAR = 0x03,
    // add two values on the stack.
    OP_IADD = 0x04,
    // subtract two values on the stack.
    OP_ISUB = 0x05,
    // multiply two values on the stack.
    OP_IMUL = 0x06,
    // divide two values on the stack.
    OP_IDIV = 0x07,

    // Separate opcodes for integer and float operations
    OP_FADD = 0x08,  // float add
    OP_FSUB = 0x09,  // float subtract
    OP_FMUL = 0x0A,  // float multiply
    OP_FDIV = 0x0B,  // float division

    // Type conversion operations
    OP_I2F = 0x0C,   // convert int to float
    OP_F2I = 0x0D,   // convert float to int (truncate)

    // conditional operations
    OP_GREATER = 0x0E,
    OP_LESS = 0x0F,
    OP_EQUAL = 0x10,
    OP_NOTEQUAL = 0x11,

    pub fn encode(op: OpCode) u8 {
        return @intFromEnum(op);
    }
    
    pub fn fromByte(byte: u8) !OpCode {
        return std.meta.intToEnum(OpCode, byte);
    }

    pub fn isBinary(self: OpCode) bool {
        return self == .OP_IADD or self == .OP_ISUB or self == .OP_IMUL or self == .OP_IDIV or self == .OP_FADD or self == .OP_FSUB or self == .OP_FMUL or self == .OP_FDIV;
    }
};

pub const ValueType = enum {
    INT,
    FLOAT,
    BOOL,
    ARRAY,
    STRING,
    ENUM,
    AUTO,
    NOTHING,
};

pub const Value = struct {
    type: ValueType,
    data: union {
        int: i64,
        float: f64,
        boolean: bool,
        array: []const Value,
        string: []const u8,
        enumeration: struct {
            value: u8,
            type: u8,
        },
        auto: *Value,
        nothing: void,
    },

};

// If you need additional data for instructions, you can use a union
pub const Instruction = union(OpCode) {
    OP_HALT: void,
    OP_CONST: Value,
    OP_VAR: Value,
    OP_IADD: void,
    OP_ISUB: void,
    OP_IMUL: void,
    OP_IDIV: void,
    OP_FADD: void,
    OP_FSUB: void,
    OP_FMUL: void,
    OP_FDIV: void,
    OP_I2F: void,
    OP_F2I: void,


    const reporter = Reporting;
    
    pub fn int(x: i64) Instruction {
        return Instruction{ 
            .OP_CONST = .{
                .type = .INT,
                .data = .{ .int = x },
            }
        };
    }

    pub fn float(x: f64) Instruction {
        return Instruction{ 
            .OP_CONST = .{
                .type = .FLOAT,
                .data = .{ .float = x },
            }
        };
    }

    pub fn boolean(x: bool) Instruction {
        return Instruction{ 
            .OP_CONST = .{
                .type = .BOOL,
                .data = .{ .bool = x },
            }
        };
    }
};

pub const InstructionSet = []const Instruction{
    Instruction.OP_HALT,
    Instruction.OP_CONST,
    Instruction.OP_IADD,
    Instruction.OP_ISUB,
    Instruction.OP_IMUL,
    Instruction.OP_IDIV,
    Instruction.OP_FADD,
    Instruction.OP_FSUB,
    Instruction.OP_FMUL,
    Instruction.OP_FDIV,
    Instruction.OP_I2F,
    Instruction.OP_F2I,


};
