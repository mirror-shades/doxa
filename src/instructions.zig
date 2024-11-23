const std = @import("std");

pub const OpCode = enum(u8) {
    // Halt the program.
    OP_HALT = 0x00,
    // pop the top value from the stack.
    OP_POP = 0x01,
    // duplicate the top value on the stack.
    OP_DUP = 0x02,

    // push a constant value into the stack.
    OP_CONST = 0x03,
    // push a variable value into the stack.
    OP_VAR = 0x04,
    // set a variable to a constant value.
    OP_SET_VAR = 0x05,
    
    // add two values on the stack.
    OP_IADD = 0x06,
    // subtract two values on the stack.
    OP_ISUB = 0x07,
    // multiply two values on the stack.
    OP_IMUL = 0x08,
    // divide two values on the stack.
    OP_IDIV = 0x09,
    // float add
    OP_FADD = 0x0A,
    // float subtract
    OP_FSUB = 0x0B,
    // float multiply
    OP_FMUL = 0x0C,
    // float division
    OP_FDIV = 0x0D,

    // Type conversion operations
    OP_I2F = 0x0E,   // convert int to float
    OP_F2I = 0x0F,   // convert float to int (truncate)

    // conditional operations
    OP_GREATER = 0x10,
    OP_LESS = 0x11,
    OP_EQUAL = 0x12,
    OP_NOTEQUAL = 0x13,

    // Jump operations
    OP_JUMP = 0x14,         // Unconditional jump
    OP_JUMP_IF = 0x15,      // Jump if top of stack is true
    OP_JUMP_IF_FALSE = 0x16,// Jump if top of stack is false

    // function operations
    OP_CALL = 0x17,
    OP_RETURN = 0x18,

    // string operations
    OP_STR_CONCAT = 0x19,      // Concatenate two strings
    OP_STR_EQ = 0x1A,      // String equality comparison
    OP_STR_LEN = 0x1B,     // Get string length
    OP_SUBSTR = 0x1C,      // Get substring

    // array operations
    OP_ARRAY_NEW = 0x1D,
    OP_ARRAY_PUSH = 0x1E,
    OP_ARRAY_LEN = 0x1F,
    OP_ARRAY_GET = 0x20,
    OP_ARRAY_SET = 0x21,
    OP_ARRAY_CONCAT = 0x22,
    OP_ARRAY_SLICE = 0x23,

    // struct operations
    OP_STRUCT_NEW = 0x24,  // Create a new struct
    OP_SET_FIELD = 0x25,   // Set a field in a struct
    OP_GET_FIELD = 0x26,   // Get a field from a struct (optional for now)

    // block operations
    OP_BEGIN_BLOCK = 0x27, // Start a new block scope   
    OP_END_BLOCK = 0x28,   // End current block scope

    // logical operations
    OP_AND = 0x29,
    OP_OR = 0x2A,
    OP_NOT = 0x2B,

    // match operations
    OP_MATCH = 0x2C,
    OP_MATCH_ARM = 0x2D,
    OP_MATCH_END = 0x2E,

    // try operations
    OP_TRY = 0x2F,         // Try an operation that might fail
    OP_CATCH = 0x30,       // Handle the error case
    OP_THROW = 0x31,       // Throw an error
    OP_END_TRY = 0x32,     // End try-catch block

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

pub const Function = struct {
    code: []const u8,
    constants: []const Value,
    arity: u32,
    name: []const u8,
};

pub const ValueType = enum {
    INT,
    FLOAT,
    BOOL,
    ARRAY,
    STRING,
    STRUCT,
    ENUM,
    AUTO,
    FUNCTION,
};

pub const ArrayValue = struct {
    items: std.ArrayList(Value),
    capacity: u32,
    
    pub fn deinit(self: *ArrayValue) void {
        self.items.deinit();
    }
};

pub const EnumValue = struct {
    type_name: []const u8,
    variant: u8,
};

pub const Value = struct {
    type: ValueType,
    nothing: bool,
    data: union {
        int: i32,
        float: f32,
        boolean: bool,
        string: []const u8,
        struct_val: *StructValue,
        array_val: *ArrayValue,
        enum_val: EnumValue,
    },

    pub fn isNumber(self: Value) bool {
        return self.type == .INT or self.type == .FLOAT;
    }

    pub fn isString(self: Value) bool {
        return self.type == .STRING;
    }

    pub fn isStruct(self: Value) bool {
        return self.type == .STRUCT;
    }

    pub fn isArray(self: Value) bool {
        return self.type == .ARRAY;
    }

    pub fn isBoolean(self: Value) bool {
        return self.type == .BOOL;
    }

    pub fn isEnum(self: Value) bool {
        return self.type == .ENUM;
    }
};

pub const StructValue = struct {
    fields: std.StringHashMap(Value),  // This expects a StringHashMap, not an array
    type_name: []const u8,
    num_fields: u8,
    
    pub fn init(allocator: std.mem.Allocator, type_name: []const u8) !*StructValue {
        const sv = try allocator.create(StructValue);
        sv.* = .{
            .fields = std.StringHashMap(Value).init(allocator),  // Initialize as HashMap
            .type_name = try allocator.dupe(u8, type_name),
            .num_fields = 0,
        };
        return sv;
    }
     
    pub fn deinit(self: *StructValue, allocator: std.mem.Allocator) void {
        var iter = self.fields.iterator();
        while (iter.next()) |entry| {
            // Recursively clean up field values
            switch (entry.value_ptr.type) {
                .STRING => allocator.free(entry.value_ptr.data.string),
                .STRUCT => entry.value_ptr.data.struct_val.deinit(allocator),
                else => {},
            }
        }
        self.fields.deinit();
        allocator.free(self.type_name);
        allocator.destroy(self);
    }
};