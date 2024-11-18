const std = @import("std");

pub const OpCode = enum(u8) {
    // Halt the program.
    OP_HALT = 0x00,

    // push a constant value into the stack.
    OP_CONST = 0x01,

        
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
    OP_CONST: u8,
    // Add more instruction variants here

    pub fn int(value: u8) Instruction {
        return Instruction{ .OP_CONST = value };
    }

};

pub const InstructionSet = []const Instruction{
    // Add all instructions here if needed
};
