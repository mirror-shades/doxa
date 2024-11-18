const std = @import("std");

pub const OpCode = enum(u8) {
    // Halt the program.
    OP_HALT = 0x00,

    // push a constant value into the stack.
    OP_CONST = 0x01,

    // add two values on the stack.
    OP_ADD = 0x02,

    // subtract two values on the stack.
    OP_SUB = 0x03,

    // multiply two values on the stack.
    OP_MUL = 0x04,

    // divide two values on the stack.
    OP_DIV = 0x05,

        
    pub fn encode(op: OpCode) u8 {
        return @intFromEnum(op);
    }
    
    pub fn fromByte(byte: u8) !OpCode {
        return std.meta.intToEnum(OpCode, byte);
    }
};

// If you need additional data for instructions, you can use a union
pub const Instruction = union(OpCode) {
    OP_HALT: void,
    OP_CONST: u32,
    OP_ADD: void,
    OP_SUB: void,
    OP_MUL: void,
    OP_DIV: void,
    // Add more instruction variants here

    pub fn int(value: u8) Instruction {
        return Instruction{ .OP_CONST = value };
    }

};

pub const InstructionSet = []const Instruction{
    Instruction.OP_HALT,
    Instruction.OP_CONST,
    Instruction.OP_ADD,
    Instruction.OP_SUB,
    Instruction.OP_MUL,
    Instruction.OP_DIV,


};
