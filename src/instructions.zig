const std = @import("std");

pub const Instruction = union(enum) {
    // Stack Manipulation
    PushInt: i64,
    PushFloat: f64,
    PushBool: bool,
    PushString: []const u8,
    Pop,
    
    // Arithmetic Operations
    Add,
    Subtract,
    Multiply,
    Divide,
    Modulo,
    Power,
    
    // Comparison Operations
    Equal,
    NotEqual,
    GreaterThan,
    LessThan,
    GreaterEqual,
    LessEqual,
    
    // Logical Operations
    And,
    Or,
    Not,
    
    // Control Flow
    Jump: usize,
    JumpIfTrue: usize,
    JumpIfFalse: usize,
    
    // Function Calls
    CallFunction: []const u8,
    Return,
    
    // Variable Operations
    LoadVariable: []const u8,
    StoreVariable: []const u8,
    
    // Miscellaneous
    Print,
    Halt,
};

pub const InstructionSet = []const Instruction{
    // Add all instructions here if needed
};
