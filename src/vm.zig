const std = @import("std");
const instructions = @import("instructions.zig");
const Reporting = @import("reporting.zig").Reporting;

const Frame = struct {
    value: instructions.Value,
    
    // Helper functions
    pub fn initInt(x: i64) Frame {
        return Frame{
            .value = instructions.Value{ .type = .INT, .data = .{ .int = x } },
        };
    }

    pub fn initFloat(x: f64) Frame {
        return Frame{
            .value = instructions.Value{ .type = .FLOAT, .data = .{ .float = x } },
        };
    }

    pub fn initString(x: []const u8) Frame {
        return Frame{
            .value = instructions.Value{ .type = .STRING, .data = .{ .string = x } },
        };
    }

    pub fn initBoolean(x: bool) Frame {
        return Frame{
            .value = instructions.Value{ .type = .BOOL, .data = .{ .boolean = x } },
        };
    }

    pub fn initNothing() Frame {
        return Frame{
            .value = instructions.Value{ .type = .NOTHING, .data = .{ .nothing = {} } },
        };
    }

    pub fn typeIs(self: Frame, expected: instructions.ValueType) bool {
        return self.value.type == expected;
    }
    
    pub fn asInt(self: Frame) !i64 {
        if (!self.typeIs(.INT)) {
            return error.TypeError;
        }
        return self.value.data.int;
    }

    pub fn asFloat(self: Frame) !f64 {
        if (!self.typeIs(.FLOAT)) {
            return error.TypeError;
        }
        return self.value.data.float;
    }

    pub fn asBoolean(self: Frame) !bool {
        if (!self.typeIs(.BOOL)) {
            return error.TypeError;
        }
        return self.value.data.boolean;
    }

    pub fn asString(self: Frame) ![]const u8 {
        if (!self.typeIs(.STRING)) {
            return error.TypeError;
        }
        return self.value.data.string;
    }
};

const STACK_SIZE = 512;
const MAX_FRAMES = 64; // Or whatever maximum call depth you want to support

const Function = struct {
    code: []const u8,         // The bytecode for this function
    constants: []const Frame, // Constants used by this function
    arity: u8,               // Number of parameters
    name: []const u8,        // Function name (for debugging)
};

const CallFrame = struct {
    ip: usize,              // Instruction pointer for this frame
    bp: usize,              // Base pointer (for local variables)
    function: *const Function, // Reference to the function being executed (made const)
    return_addr: usize,     // Where to return to in the caller's code
};

pub const VM = struct {
    allocator: std.mem.Allocator,
    code: []const u8,
    stack: [STACK_SIZE]Frame,
    constants: []const Frame,
    variables: std.ArrayList(Frame),
    frames: std.ArrayList(CallFrame),
    frame_count: usize,
    sp: usize,
    ip: usize,
    reporter: *Reporting,

    pub fn init(allocator: std.mem.Allocator, code: []const u8, constants: []const Frame, reporter: *Reporting) VM {
        var stack: [STACK_SIZE]Frame = undefined;
        // Initialize all stack elements with NOTHING
        for (&stack) |*slot| {
            slot.* = Frame.initNothing();
        }

        return VM{
            .allocator = allocator,
            .code = code,
            .stack = stack,
            .constants = constants,
            .variables = std.ArrayList(Frame).init(allocator),
            .frames = std.ArrayList(CallFrame).init(allocator),
            .frame_count = 0,
            .ip = 0,
            .sp = 0,
            .reporter = reporter,
        };
    }

    pub fn deinit(self: *VM) void {
        self.variables.deinit();
        self.frames.deinit();
    }

    fn push(self: *VM, value: Frame) void {
        if (self.sp >= STACK_SIZE) {
            self.reporter.reportFatalError("Stack overflow", .{});
            return;
        }
        self.stack[self.sp] = value;
        self.sp += 1;
    }

    fn pop(self: *VM) ?Frame {
        if (self.sp == 0) {
            self.reporter.reportFatalError("Stack underflow", .{});
            return null;
        }
        self.sp -= 1;
        return self.stack[self.sp];
    }

    fn get_constant(self: *VM, index: u8) ?Frame {
        if (index >= self.constants.len) {
            self.reporter.reportFatalError("Constant index out of bounds", .{});
            return null;
        }
        return self.constants[index];
    }

    fn eval(self: *VM) !?Frame {
        while (true) {
            const opcode: instructions.OpCode = @enumFromInt(self.read_byte());
            std.debug.print("Opcode: {}, Stack size: {}\n", .{opcode, self.sp});
            switch (opcode) {
                // halt
                instructions.OpCode.OP_HALT => {
                    // Return the top value of the stack instead of null
                    return if (self.sp > 0) self.stack[self.sp - 1] else Frame.initNothing();
                },
                // push constant
                instructions.OpCode.OP_CONST => {
                    const constant_index = self.read_byte();
                    const constant_value = self.get_constant(constant_index) orelse return null;
                    self.push(constant_value);
                },
                // push variable - implementation depends on your variable system
                instructions.OpCode.OP_VAR => {
                    const var_index = self.read_byte();
                    if (var_index >= self.variables.items.len) {
                        self.reporter.reportFatalError("Variable index out of bounds", .{});
                        return null;
                    }
                    self.push(self.variables.items[var_index]);
                },
                // add
                instructions.OpCode.OP_IADD, 
                instructions.OpCode.OP_ISUB,
                instructions.OpCode.OP_IMUL,
                instructions.OpCode.OP_IDIV => {
                    const b = self.pop().?;
                    const a = self.pop().?;
                    
                    // Fast path: both integers
                    if (a.value.type == .INT and b.value.type == .INT) {
                        const result = switch (opcode) {
                            .OP_IADD => i_add(a.asInt() catch return null, b.asInt() catch return null),
                            .OP_ISUB => i_sub(a.asInt() catch return null, b.asInt() catch return null),
                            .OP_IMUL => i_mul(a.asInt() catch return null, b.asInt() catch return null),
                            .OP_IDIV => blk: {
                                // Always convert to floats for division
                                const a_float = @as(f64, @floatFromInt(a.asInt() catch return null));
                                const b_float = @as(f64, @floatFromInt(b.asInt() catch return null));
                                break :blk f_div(a_float, b_float) catch return null;
                            },
                            else => unreachable,
                        } catch return null;
                        self.push(result);
                        continue;
                    }
                    
                    // Mixed types: convert to floats
                    switch (a.value.type) {
                        .INT => switch (b.value.type) {
                            .FLOAT => {
                                const a_float = @as(f64, @floatFromInt(a.asInt() catch return null));
                                const result = switch (opcode) {
                                    .OP_IADD => f_add(a_float, b.asFloat() catch return null),
                                    .OP_ISUB => f_sub(a_float, b.asFloat() catch return null),
                                    .OP_IMUL => f_mul(a_float, b.asFloat() catch return null),
                                    .OP_IDIV => f_div(a_float, b.asFloat() catch return null),
                                    else => unreachable,
                                } catch return null;
                                self.push(result);
                            },
                            else => {
                                self.reporter.reportFatalError("Invalid type for arithmetic operation", .{});
                                return null;
                            },
                        },
                        .FLOAT => switch (b.value.type) {
                            .INT => {
                                const b_float = @as(f64, @floatFromInt(b.asInt() catch return null));
                                const result = switch (opcode) {
                                    .OP_IADD => f_add(a.asFloat() catch return null, b_float),
                                    .OP_ISUB => f_sub(a.asFloat() catch return null, b_float),
                                    .OP_IMUL => f_mul(a.asFloat() catch return null, b_float),
                                    .OP_IDIV => f_div(a.asFloat() catch return null, b_float),
                                    else => unreachable,
                                } catch return null;
                                self.push(result);
                            },
                            .FLOAT => {
                                const result = switch (opcode) {
                                    .OP_IADD => f_add(a.asFloat() catch return null, b.asFloat() catch return null),
                                    .OP_ISUB => f_sub(a.asFloat() catch return null, b.asFloat() catch return null),
                                    .OP_IMUL => f_mul(a.asFloat() catch return null, b.asFloat() catch return null),
                                    .OP_IDIV => f_div(a.asFloat() catch return null, b.asFloat() catch return null),
                                    else => unreachable,
                                } catch return null;
                                self.push(result);
                            },
                            else => {
                                self.reporter.reportFatalError("Invalid type for arithmetic operation", .{});
                                return null;
                            },
                        },
                        else => {
                            self.reporter.reportFatalError("Invalid type for arithmetic operation", .{});
                            return null;
                        },
                    }
                },
                instructions.OpCode.OP_FADD,
                instructions.OpCode.OP_FSUB,
                instructions.OpCode.OP_FMUL,
                instructions.OpCode.OP_FDIV => {
                    const b = self.pop().?;
                    const a = self.pop().?;

                    // Convert operands to float if needed
                    const a_val: f64 = switch (a.value.type) {
                        .FLOAT => a.asFloat() catch return null,
                        .INT => @as(f64, @floatFromInt(a.asInt() catch return null)),
                        else => {
                            self.reporter.reportFatalError("Invalid type for float operation", .{});
                            return null;
                        },
                    };

                    const b_val: f64 = switch (b.value.type) {
                        .FLOAT => b.asFloat() catch return null,
                        .INT => @as(f64, @floatFromInt(b.asInt() catch return null)),
                        else => {
                            self.reporter.reportFatalError("Invalid type for float operation", .{});
                            return null;
                        },
                    };

                    const result = switch (opcode) {
                        .OP_FADD => f_add(a_val, b_val),
                        .OP_FSUB => f_sub(a_val, b_val),
                        .OP_FMUL => f_mul(a_val, b_val),
                        .OP_FDIV => f_div(a_val, b_val),
                        else => unreachable,
                    } catch return null;
                    self.push(result);
                },  
                // greater
                instructions.OpCode.OP_GREATER => {
                    const b = self.pop().?;
                    const a = self.pop().?;
                    const result = greater(a, b) catch return null;
                    self.push(result);
                },
                // less
                instructions.OpCode.OP_LESS => {
                    const b = self.pop().?;
                    const a = self.pop().?;
                    const result = less(a, b) catch return null;
                    self.push(result);
                },
                // i2f
                instructions.OpCode.OP_I2F => {
                    const value = self.pop() orelse return null;
                    std.debug.print("I2F: Popped value type: {}\n", .{value.value.type});
                    if (!value.typeIs(.INT)) {
                        self.reporter.reportFatalError("Expected integer for I2F conversion", .{});
                        return null;
                    }
                    const int_val = value.asInt() catch return null;
                    const float_val = @as(f64, @floatFromInt(int_val));
                    self.push(Frame.initFloat(float_val));
                    std.debug.print("I2F: Converted {} to {d}\n", .{int_val, float_val});
                },
                // f2i
                instructions.OpCode.OP_F2I => {
                    const value = self.pop() orelse return null;
                    const float_val = value.asFloat() catch return null;
                    const int_val = @as(i64, @intFromFloat(std.math.trunc(float_val)));
                    self.push(Frame.initInt(int_val));
                },
                // equal
                instructions.OpCode.OP_EQUAL => {
                    const b = self.pop().?;
                    const a = self.pop().?;
                    const result = equal(a, b) catch return null;
                    self.push(result);
                },
                // notequal
                instructions.OpCode.OP_NOTEQUAL => {
                    const b = self.pop().?;
                    const a = self.pop().?;
                    const result = notEqual(a, b) catch return null;
                    self.push(result);
                },
                // set variable
                instructions.OpCode.OP_SET_VAR => {
                    const var_index = self.read_byte();
                    const value = self.pop() orelse return null;
                    
                    // Grow variables array if needed
                    while (var_index >= self.variables.items.len) {
                        self.variables.append(Frame.initNothing()) catch {
                            self.reporter.reportFatalError("Failed to allocate variable", .{});
                            return null;
                        };
                    }
                    
                    self.variables.items[var_index] = value;
                },
                instructions.OpCode.OP_JUMP => {
                    const jump_offset = self.read_byte();
                    self.ip += jump_offset;
                },
                instructions.OpCode.OP_JUMP_IF => {
                    const jump_address = self.read_byte();
                    const condition = self.pop().?;
                    if (condition.value.type != .BOOL) {
                        self.reporter.reportFatalError("Expected boolean for JUMP_IF", .{});
                        return null;
                    }
                    const bool_value = condition.asBoolean() catch return null;
                    if (bool_value) {
                        self.ip += jump_address;
                    }
                },
                instructions.OpCode.OP_JUMP_IF_FALSE => {
                    const jump_address = self.read_byte();
                    const condition = self.pop().?;
                    if (condition.value.type != .BOOL) {
                        self.reporter.reportFatalError("Expected boolean for JUMP_IF_FALSE", .{});
                        return null;
                    }
                    const bool_value = condition.asBoolean() catch return null;
                    if (!bool_value) {
                        self.ip += jump_address;
                    }
                },
                instructions.OpCode.OP_RETURN => {
                    if (try self.executeReturn()) |final_return| {
                        return final_return;
                    } else {
                        self.reporter.reportFatalError("Returned null", .{});
                        return null;
                    }
                },
            }

            // Debug print stack after each operation
            std.debug.print("Stack after operation: ", .{});
            var i: usize = 0;
            while (i < self.sp) : (i += 1) {
                switch (self.stack[i].value.type) {
                    .INT => std.debug.print("INT({}) ", .{self.stack[i].value.data.int}),
                    .FLOAT => std.debug.print("FLOAT({d}) ", .{self.stack[i].value.data.float}),
                    .STRING => std.debug.print("STRING({s}) ", .{self.stack[i].value.data.string}),
                    .BOOL => std.debug.print("BOOL({}) ", .{self.stack[i].value.data.boolean}),
                    .NOTHING => std.debug.print("NOTHING ", .{}),
                    else => std.debug.print("UNKNOWN ", .{}),
                }
            }
            std.debug.print("\n", .{});
        }
    }

    fn read_byte(self: *VM) u8 {
        const byte = self.code[self.ip];
        self.ip += 1;
        return byte;
    }

    fn i2f(self: *VM, a: i64) !void {
        const result: f64 = @as(f64, @floatFromInt(a));
        self.push(Frame.initFloat(result));
    }

    fn f2i(self: *VM, a: f64) !void {
        const result: i64 = @intFromFloat(std.math.trunc(a));
        self.push(Frame.initInt(result));
    }

   fn i_add(a: i64, b: i64) !Frame {
        if (b > 0 and a > std.math.maxInt(i64) - b) {
            return error.IntegerOverflow;
        }
        if (b < 0 and a < std.math.minInt(i64) - b) {
            return error.IntegerOverflow;
        }
        return Frame.initInt(a + b);
    }

    fn i_sub(a: i64, b: i64) !Frame {
        // Check for overflow: if b is positive, a must not be less than MIN + b
        // if b is negative, a must not be greater than MAX + b
        if (b > 0 and a < std.math.minInt(i64) + b) {
            return error.IntegerOverflow;
        }
        if (b < 0 and a > std.math.maxInt(i64) + b) {
            return error.IntegerOverflow;
        }
        return Frame.initInt(a - b);
    }

    fn i_mul(a: i64, b: i64) !Frame {
        // Special cases first
        if (a == 0 or b == 0) return Frame.initInt(0);
        if (a == 1) return Frame.initInt(b);
        if (b == 1) return Frame.initInt(a);

        // Check for overflow
        if (a > 0) {
            if (b > 0) {
                if (a > @divTrunc(std.math.maxInt(i64), b)) return error.IntegerOverflow;
            } else {
                if (b < @divTrunc(std.math.minInt(i64), a)) return error.IntegerOverflow;
            }
        } else {
            if (b > 0) {
                if (a < @divTrunc(std.math.minInt(i64), b)) return error.IntegerOverflow;
            } else {
                if (a != 0 and b < @divTrunc(std.math.maxInt(i64), a)) return error.IntegerOverflow;
            }
        }
        return Frame.initInt(a * b);
    }

    fn f_add(a: f64, b: f64) !Frame {
        if (a + b < -std.math.floatMax(f64) or a + b > std.math.floatMax(f64)) {
            return error.FloatOverflow;
        }
        return Frame.initFloat(a + b);
    }

    fn f_sub(a: f64, b: f64) !Frame {
        if (a - b < -std.math.floatMax(f64) or a - b > std.math.floatMax(f64)) {
            return error.FloatOverflow;
        }
        return Frame.initFloat(a - b);
    }

    fn f_mul(a: f64, b: f64) !Frame {
        if (a * b < -std.math.floatMax(f64) or a * b > std.math.floatMax(f64)) {
            return error.FloatOverflow;
        }
        return Frame.initFloat(a * b);
    }

    fn f_div(a: f64, b: f64) !Frame {
        if (b == 0) {
            return error.DivisionByZero;
        }
        return Frame.initFloat(a / b);
    }

    fn greater(a: Frame, b: Frame) !Frame {
    // If both are the same type, compare directly
    if (a.value.type == b.value.type) {
        return switch (a.value.type) {
            .INT => {
                const a_val = try a.asInt();
                const b_val = try b.asInt();
                return Frame.initBoolean(a_val > b_val);
            },
            .FLOAT => {
                const a_val = try a.asFloat();
                const b_val = try b.asFloat();
                return Frame.initBoolean(a_val > b_val);
            },
            else => error.TypeError,
        };
    }

    // Handle mixed INT/FLOAT comparisons
    if ((a.value.type == .INT and b.value.type == .FLOAT) or 
        (a.value.type == .FLOAT and b.value.type == .INT)) {
        // Convert both to floats for comparison
        const a_val: f64 = if (a.value.type == .INT)
            @as(f64, @floatFromInt(try a.asInt()))
        else
            try a.asFloat();

        const b_val: f64 = if (b.value.type == .INT)
            @as(f64, @floatFromInt(try b.asInt()))
        else
            try b.asFloat();

        return Frame.initBoolean(a_val > b_val);
    }

    return error.TypeError;
    }

    fn less(a: Frame, b: Frame) !Frame {
        // If both are the same type, compare directly
        if (a.value.type == b.value.type) {
            return switch (a.value.type) {
                .INT => {
                    const a_val = try a.asInt();
                    const b_val = try b.asInt();
                    return Frame.initBoolean(a_val < b_val);
                },
                .FLOAT => {
                    const a_val = try a.asFloat();
                    const b_val = try b.asFloat();
                    return Frame.initBoolean(a_val < b_val);
                },
                else => error.TypeError,
            };
        }

        // Handle mixed INT/FLOAT comparisons
        if ((a.value.type == .INT and b.value.type == .FLOAT) or 
            (a.value.type == .FLOAT and b.value.type == .INT)) {
            // Convert both to floats for comparison
            const a_val: f64 = if (a.value.type == .INT)
                @as(f64, @floatFromInt(try a.asInt()))
            else
                try a.asFloat();

            const b_val: f64 = if (b.value.type == .INT)
                @as(f64, @floatFromInt(try b.asInt()))
            else
                try b.asFloat();

            return Frame.initBoolean(a_val < b_val);
        }

        return error.TypeError;
    }

    fn equal(a: Frame, b: Frame) !Frame {
        // If both are the same type, compare directly
        if (a.value.type == b.value.type) {
            return switch (a.value.type) {
                .INT => {
                    const a_val = try a.asInt();
                    const b_val = try b.asInt();
                    return Frame.initBoolean(a_val == b_val);
                },
                .FLOAT => {
                    const a_val = try a.asFloat();
                    const b_val = try b.asFloat();
                    return Frame.initBoolean(a_val == b_val);
                },
                .STRING => {
                    const a_val = try a.asString();
                    const b_val = try b.asString();
                    return Frame.initBoolean(std.mem.eql(u8, a_val, b_val));
                },
                .BOOL => {
                    const a_val = try a.asBoolean();
                    const b_val = try b.asBoolean();
                    return Frame.initBoolean(a_val == b_val);
                },
                .NOTHING => return Frame.initBoolean(true), // NOTHING equals NOTHING
                else => error.TypeError,
            };
        }

        // Handle mixed INT/FLOAT comparisons
        if ((a.value.type == .INT and b.value.type == .FLOAT) or 
            (a.value.type == .FLOAT and b.value.type == .INT)) {
            // Convert both to floats for comparison
            const a_val: f64 = if (a.value.type == .INT)
                @as(f64, @floatFromInt(try a.asInt()))
            else
                try a.asFloat();

            const b_val: f64 = if (b.value.type == .INT)
                @as(f64, @floatFromInt(try b.asInt()))
            else
                try b.asFloat();

            return Frame.initBoolean(a_val == b_val);
        }

        // In normal mode, different types are never equal (Python-like behavior)
        return Frame.initBoolean(false);
    }

    fn notEqual(a: Frame, b: Frame) !Frame {
        const result = try equal(a, b);
        return Frame.initBoolean(!(try result.asBoolean()));
    }

    fn halt(self: *VM) !void {
        self.push(Frame.initNothing());
    }

    fn executeReturn(self: *VM) !?Frame {
        // 1. Validate stack state
        if (self.sp == 0) {
            return error.StackUnderflow;
        } 

        // 2. Get the return value from top of stack
        const return_value = self.pop() orelse return error.StackUnderflow;
        
        // 3. Handle top-level return
        if (self.frame_count == 0) {
            // Ensure stack is empty except for return value
            if (self.sp != 0) {
                self.reporter.reportWarning("Stack not empty at top-level return", .{});
                // Clean up remaining stack
                self.sp = 0;
            }
            return return_value;
        }
        
        // 4. Get current frame
        if (self.frame_count > self.frames.items.len) {
            return error.FrameStackCorruption;
        }
        const current_frame = &self.frames.items[self.frame_count - 1];
        
        // 5. Validate frame state
        if (current_frame.bp > self.sp) {
            return error.FrameStackMismatch;
        }
        
        // 6. Clean up current frame's stack
        self.sp = current_frame.bp; // Reset stack pointer to base pointer
        
        // 7. Push return value to previous frame's stack
        self.push(return_value);
        
        // 8. Restore previous frame
        self.frame_count -= 1;
        if (self.frame_count > 0) {
            const prev_frame = &self.frames.items[self.frame_count - 1];
            if (prev_frame.return_addr >= self.code.len) {
                return error.InvalidReturnAddress;
            }
            self.ip = prev_frame.return_addr;
        }
        
        return null; // Continue execution
    }

};



pub fn main() !void {
    var gpa = std.heap.GeneralPurposeAllocator(.{}){};
    const allocator = gpa.allocator();
    defer _ = gpa.deinit();

    // Example constants
    const constants = [_]Frame{
        Frame.initBoolean(false),    // constant[0]: true
        Frame.initInt(42),          // constant[1]: 42
        Frame.initInt(24),          // constant[2]: 24
    };

    // Example bytecode for:
    // if (true) { 
    //     return 42; 
    // } else { 
    //     return 24; 
    // }
    const code = [_]u8{
        @intFromEnum(instructions.OpCode.OP_CONST), 0,    // Push true
        @intFromEnum(instructions.OpCode.OP_JUMP_IF_FALSE), 4,  // Jump to else branch if false (skip next 4 bytes)
        @intFromEnum(instructions.OpCode.OP_CONST), 1,    // Push 42
        @intFromEnum(instructions.OpCode.OP_JUMP), 2,     // Jump to end (skip next 2 bytes)
        @intFromEnum(instructions.OpCode.OP_CONST), 2,    // Push 24 (else branch)
        @intFromEnum(instructions.OpCode.OP_HALT),        // End program
    };

    var reporter = Reporting.initStderr();
    var vm = VM.init(allocator, &code, &constants, &reporter);
    defer vm.deinit();
    
    const result = try vm.eval();
    
    if (result) |frame| {
        switch (frame.value.type) {
            .INT => std.debug.print("Result: {}\n", .{frame.value.data.int}), // Should print 42
            else => std.debug.print("Unexpected result type\n", .{}),
        }
    } else {
        std.debug.print("VM halted with empty stack\n", .{});
    }
}


           