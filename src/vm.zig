const std = @import("std");
const instructions = @import("instructions.zig");
const Reporting = @import("reporting.zig").Reporting;

const Frame = struct {
    value: instructions.Value,
    allocator: ?std.mem.Allocator,
    
    // Helper functions
    pub fn initInt(x: i64) Frame {
        return Frame{
            .value = instructions.Value{ .type = .INT, .data = .{ .int = x } },
            .allocator = null,
        };
    }

    pub fn initFloat(x: f64) Frame {
        return Frame{
            .value = instructions.Value{ .type = .FLOAT, .data = .{ .float = x } },
            .allocator = null,
        };
    }

    pub fn initString(allocator: std.mem.Allocator, x: []const u8) !Frame {
        const duped_string = try allocator.dupe(u8, x);
        return Frame{
            .value = instructions.Value{ .type = .STRING, .data = .{ .string = duped_string } },
            .allocator = allocator,
        };
    }

    pub fn initBoolean(x: bool) Frame {
        return Frame{
            .value = instructions.Value{ .type = .BOOL, .data = .{ .boolean = x } },
            .allocator = null,
        };
    }

    pub fn initNothing() Frame {
        return Frame{
            .value = instructions.Value{ .type = .NOTHING, .data = .{ .nothing = {} } },
            .allocator = null,
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

    pub fn deinit(self: *Frame) void {
        if (self.allocator) |alloc| {
            switch (self.value.type) {
                .STRING => alloc.free(self.value.data.string),
                else => {},
            }
        }
    }
};

const STACK_SIZE = 512;
const MAX_FRAMES = 64; // Or whatever maximum call depth you want to support


const CallFrame = struct {
    ip: usize,              // Instruction pointer for this frame
    bp: usize,              // Base pointer (start of this frame's stack space)
    sp: usize,              // Stack pointer for this frame
    function: *const instructions.Function, // Reference to the function being executed
    return_addr: usize,     // Where to return to in the caller's code
};

pub const VM = struct {
    allocator: std.mem.Allocator,
    code: []const u8,
    stack: [STACK_SIZE]Frame,
    constants: []Frame,
    variables: std.ArrayList(Frame),
    functions: std.ArrayList(*instructions.Function),
    frames: std.ArrayList(CallFrame),
    frame_count: usize,
    sp: usize,
    ip: usize,
    reporter: *Reporting,
    call_stack: std.ArrayList(CallFrame),
    current_frame: ?*CallFrame,  // Points to the active frame
    global_constants: []instructions.Value,  // Add this field to track global constants

    pub fn init(allocator: std.mem.Allocator, code: []const u8, constants: []Frame, reporter: *Reporting) VM {
        var vm = VM{
            .allocator = allocator,
            .code = code,
            .stack = undefined,
            .constants = constants,
            .variables = std.ArrayList(Frame).init(allocator),
            .functions = std.ArrayList(*instructions.Function).init(allocator),
            .frames = std.ArrayList(CallFrame).init(allocator),
            .frame_count = 0,
            .ip = 0,
            .sp = 0,
            .reporter = reporter,
            .call_stack = std.ArrayList(CallFrame).init(allocator),
            .current_frame = null,
            .global_constants = allocator.alloc(instructions.Value, constants.len) catch unreachable,
        };

        // Initialize stack with NOTHING values
        for (&vm.stack) |*slot| {
            slot.* = Frame.initNothing();
        }
        
        // Initialize global constants
        for (constants, 0..) |constant, i| {
            vm.global_constants[i] = constant.value;
        }

        // Allocate the global function in memory
        const global_function = allocator.create(instructions.Function) catch unreachable;
        global_function.* = .{
            .arity = 0,
            .code = code,
            .constants = vm.global_constants,
            .name = "global",
        };

        // Push the initial frame
        vm.call_stack.append(CallFrame{
            .ip = 0,
            .bp = 0,
            .sp = 0,
            .function = global_function,
            .return_addr = 0,
        }) catch unreachable;
        
        vm.current_frame = &vm.call_stack.items[0];

        return vm;
    }

    pub fn deinit(self: *VM) void {
        // Clean up stack frames
        for (self.stack[0..self.sp]) |*frame| {
            frame.deinit();
        }
        
        // Clean up constants
        for (self.constants) |*constant| {
            constant.deinit();
        }
        
        // Free the global function that was created in init
        if (self.call_stack.items.len > 0) {
            const global_frame = self.call_stack.items[0];
            self.allocator.destroy(global_frame.function);
        }

        // Free other resources
        self.allocator.free(self.global_constants);
        self.variables.deinit();
        self.functions.deinit();
        self.frames.deinit();
        self.call_stack.deinit();
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
        if (self.current_frame) |frame| {
            // Use the current frame's function constants
            if (index >= frame.function.constants.len) {
                self.reporter.reportFatalError("Constant index out of bounds", .{});
                return null;
            }
            const constant_value = frame.function.constants[index];
            return Frame{ 
                .value = constant_value,
                .allocator = null,  // Constants don't need allocation
            };
        } else {
            // Fallback to global constants
            if (index >= self.constants.len) {
                self.reporter.reportFatalError("Constant index out of bounds", .{});
                return null;
            }
            return self.constants[index];
        }
    }

    pub fn eval(self: *VM) !?Frame {
        while (true) {
            // Get instruction from current frame
            if (self.current_frame) |frame| {
                if (frame.ip >= frame.function.code.len) {
                    return error.UnexpectedEndOfCode;
                }
                // Important: Use the frame's function code, not self.code
                const opcode: instructions.OpCode = @enumFromInt(frame.function.code[frame.ip]);
                frame.ip += 1;
                self.ip = frame.ip;  // Keep global IP in sync

                // Debug print
                std.debug.print("Executing instruction at {}: {}\n", .{frame.ip - 1, opcode});

                switch (opcode) {
                    .OP_HALT => {
                        if (self.sp > 0) {
                            return self.pop();
                        }
                        return Frame.initInt(0);
                    },
                    .OP_CONST => {
                    const constant_index = self.read_byte();
                    const constant_value = self.get_constant(constant_index) orelse return null;
                    self.push(constant_value);
                },
                // push variable - implementation depends on your variable system
                instructions.OpCode.OP_VAR => {
                    const var_index = self.read_byte();
                    
                    // If we're in a function call
                    if (self.current_frame) |func_frame| {
                        // Access the argument relative to the frame's base pointer
                        const arg_position = func_frame.bp + var_index;
                        if (arg_position >= self.sp) {
                            self.reporter.reportFatalError("Variable index out of bounds", .{});
                            return null;
                        }
                        self.push(self.stack[arg_position]);
                    } else {
                        // Original global variable logic
                        if (var_index >= self.variables.items.len) {
                            self.reporter.reportFatalError("Variable index out of bounds", .{});
                            return null;
                        }
                        self.push(self.variables.items[var_index]);
                    }
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
                    }
                    continue;
                },
                instructions.OpCode.OP_CALL => {
                    const function_index = self.read_byte();
                    const function = self.getFunction(function_index) orelse return null;
                    
                    std.debug.print("Calling function {} with {} arguments\n", .{function_index, function.arity});
                    self.printStack();  // Debug print stack before call
                    
                    // Create new frame
                    try self.call_stack.append(CallFrame{
                        .ip = 0,
                        .bp = self.sp - function.arity,  // Set base pointer to point to the first argument
                        .sp = self.sp,
                        .function = function,
                        .return_addr = self.ip,
                    });
                    
                    self.current_frame = &self.call_stack.items[self.call_stack.items.len - 1];
                    continue;
                },  
                instructions.OpCode.OP_CONCAT => {
                    const b = self.pop().?;
                    const a = self.pop().?;
                    const result = concat(self.allocator, a, b) catch return null;
                    self.push(result);
                },
                instructions.OpCode.OP_STR_EQ => {
                    const b = self.pop().?;
                    const a = self.pop().?;
                    const result = str_eq(a, b) catch return null;
                    self.push(result);
                },
                instructions.OpCode.OP_STR_LEN => {
                    const a = self.pop().?;
                    const result = str_len(a) catch return null;
                    self.push(result);
                },
                instructions.OpCode.OP_SUBSTR => {
                    const start = self.read_byte();
                    const len = self.read_byte();
                    var a = self.pop().?;
                    const result = substr(self.allocator, a, start, len) catch return null;
                    a.deinit();
                    self.push(result);
                },
            }

            // Your existing debug print code
            std.debug.print("Stack after operation: ", .{});
            var i: usize = 0;
            while (i < self.sp) : (i += 1) {
            // ... rest of debug printing ...
            }
            std.debug.print("\n", .{});
            } else {
                return error.NoActiveFrame;
            }
        }
    }

    fn read_byte(self: *VM) u8 {
        if (self.current_frame) |frame| {
            const byte = frame.function.code[frame.ip];
            frame.ip += 1;
            self.ip = frame.ip;  // Keep global IP in sync
            return byte;
        }
        @panic("No active frame");
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
        std.debug.print("\nExecuting return:\n", .{});
        self.printStack();  // Debug print stack before return
        
        // Get return value from top of stack
        const return_value = self.pop() orelse return error.EmptyStack;
        
        // If we're in the global frame, return the value
        if (self.call_stack.items.len <= 1) {
            return return_value;
        }
        
        // Pop current frame
        _ = self.call_stack.pop();
        self.current_frame = &self.call_stack.items[self.call_stack.items.len - 1];
        
        // Restore caller's stack pointer but keep one slot for return value
        self.sp = self.current_frame.?.bp;
        self.push(return_value);
        
        // Restore caller's instruction pointer
        self.ip = self.current_frame.?.return_addr;
        
        self.printStack();  // Debug print stack after return
        return null;  // Continue execution
    }

    fn getFunction(self: *VM, index: u8) ?*instructions.Function {
        if (index >= self.functions.items.len) {
            self.reporter.reportFatalError("Function index out of bounds", .{});
            return null;
        }
        return self.functions.items[index];
    }

    fn callFunction(self: *VM, function: *instructions.Function) !void {
        if (self.frame_count >= self.frames.items.len) {
            self.reporter.reportFatalError("Frame stack overflow", .{});
            return error.FrameStackOverflow;
        }

        try self.frames.append(CallFrame{
            .ip = self.ip,
            .bp = self.sp - function.arity,
            .function = function,
            .return_addr = self.ip,
        });
        self.frame_count += 1;
        self.ip = 0;
    }

    fn pushFrame(self: *VM, function: *const instructions.Function) !void {
        const frame = try self.call_stack.addOne();
        frame.* = CallFrame{
            .ip = 0,
            .bp = self.sp - function.arity,  // Stack space starts after arguments
            .sp = self.sp,
            .function = function,
            .return_addr = if (self.current_frame) |cf| cf.ip else 0,
        };
        self.current_frame = frame;
    }

    fn popFrame(self: *VM) !void {
        if (self.call_stack.items.len <= 1) {
            return error.CannotPopGlobalFrame;
        }
        
        _ = self.call_stack.pop();
        self.current_frame = &self.call_stack.items[self.call_stack.items.len - 1];
        self.ip = self.current_frame.?.return_addr;
    }

    fn concat(allocator: std.mem.Allocator, a: Frame, b: Frame) !Frame {
        if (a.value.type == .STRING and b.value.type == .STRING) {
            const a_str = try a.asString();
            const b_str = try b.asString();
            
            var result = try std.ArrayList(u8).initCapacity(allocator, a_str.len + b_str.len);
            try result.appendSlice(a_str);
            try result.appendSlice(b_str);
            
            // Create frame with the concatenated string
            const frame = try Frame.initString(allocator, result.items);
            result.deinit();
            return frame;
        }
        return error.TypeError;
    }

    fn str_eq(a: Frame, b: Frame) !Frame {
        if (a.value.type == .STRING and b.value.type == .STRING) {
            const a_str = try a.asString();
            const b_str = try b.asString();
            return Frame.initBoolean(std.mem.eql(u8, a_str, b_str));
        }
        return error.TypeError;
    }

    fn str_len(a: Frame) !Frame {
        if (a.value.type == .STRING) {  
            const a_str = try a.asString();
            return Frame.initInt(@as(i64, @intCast(a_str.len)));
        }
        return error.TypeError;
    }

    fn substr(allocator: std.mem.Allocator, a: Frame, start: u8, len: u8) !Frame {
        if (a.value.type == .STRING) {
            const a_str = try a.asString();
            const start_idx = @as(usize, start);
            const length = @as(usize, len);
            
            // Add bounds checking
            if (start_idx >= a_str.len or start_idx + length > a_str.len) {
                return error.IndexOutOfBounds;
            }
            
            // Create a new string with the substring
            return Frame.initString(allocator, a_str[start_idx..][0..length]);
        }
        return error.TypeError;
    }

    // Helper methods for stack operations relative to current frame
    fn pushValue(self: *VM, value: Frame) !void {
        if (self.sp >= STACK_SIZE) {
            return error.StackOverflow;
        }
        self.stack[self.sp] = value;
        self.sp += 1;
        if (self.current_frame) |frame| {
            frame.sp = self.sp;
        }
    }

    fn popValue(self: *VM) !Frame {
        if (self.current_frame) |frame| {
            if (self.sp <= frame.bp) {
                return error.StackUnderflow;
            }
        }
        self.sp -= 1;
        if (self.current_frame) |frame| {
            frame.sp = self.sp;
        }
        return self.stack[self.sp];
    }

    fn executeCall(self: *VM, function_index: u8) !void {
        const function = self.getFunction(function_index) orelse return error.InvalidFunction;
        
        // Validate we have enough arguments on the stack
        if (self.current_frame) |frame| {
            if (self.sp - frame.bp < function.arity) {
                return error.InsufficientArguments;
            }
        }

        try self.pushFrame(function);
    }

    // Add debug printing for stack contents
    fn printStack(self: *VM) void {
        std.debug.print("Stack contents ({} items):\n", .{self.sp});
        var i: usize = 0;
        while (i < self.sp) : (i += 1) {
            const value = self.stack[i].value;
            switch (value.type) {
                .INT => std.debug.print("  [{}] INT: {}\n", .{i, value.data.int}),
                .FLOAT => std.debug.print("  [{}] FLOAT: {}\n", .{i, value.data.float}),
                .BOOL => std.debug.print("  [{}] BOOL: {}\n", .{i, value.data.boolean}),
                .STRING => std.debug.print("  [{}] STRING: {s}\n", .{i, value.data.string}),
                .NOTHING => std.debug.print("  [{}] NOTHING\n", .{i}),
                else => std.debug.print("  [{}] OTHER\n", .{i}),
            }
        }
    }

};



pub fn main() !void {
    var gpa = std.heap.GeneralPurposeAllocator(.{}){};
    const allocator = gpa.allocator();
    defer {
        const leaked = gpa.deinit();
        if (leaked == .leak) @panic("Memory leak detected!");
    }

    // Create an array to store our constants
    var constants_array = [_]Frame{
        try Frame.initString(allocator, "Hello"),    // constant[0]
        try Frame.initString(allocator, " World"),   // constant[1]
        try Frame.initString(allocator, "Hello"),    // constant[2]
    };
    
    // No need for separate defer block as VM will handle cleanup

    // Test program that exercises all string operations
    const code = [_]u8{
        // Test concatenation: "Hello" + " World"
        @intFromEnum(instructions.OpCode.OP_CONST), 0,    // Push "Hello"
        @intFromEnum(instructions.OpCode.OP_CONST), 1,    // Push " World"
        @intFromEnum(instructions.OpCode.OP_CONCAT),      // Concatenate
        
        // Test string equality: "Hello" == "Hello"
        @intFromEnum(instructions.OpCode.OP_CONST), 0,    // Push "Hello"
        @intFromEnum(instructions.OpCode.OP_CONST), 2,    // Push "Hello" again
        @intFromEnum(instructions.OpCode.OP_STR_EQ),      // Compare equality
        
        // Test string length: len("Hello")
        @intFromEnum(instructions.OpCode.OP_CONST), 0,    // Push "Hello"
        @intFromEnum(instructions.OpCode.OP_STR_LEN),     // Get length
        
        // Test substring: "Hello"[1:3] (should get "el")
        @intFromEnum(instructions.OpCode.OP_CONST), 0,    // Push "Hello"
        @intFromEnum(instructions.OpCode.OP_SUBSTR), 1, 2,// Get substring from index 1, length 2
        
        @intFromEnum(instructions.OpCode.OP_HALT),
    };

    var reporter = Reporting.initStderr();
    var vm = VM.init(allocator, &code, &constants_array, &reporter);
    defer vm.deinit();

    // Run the VM and print results after each operation
    var ip: usize = 0;
    while (ip < code.len) {
        if (try vm.eval()) |mut_frame| {  // Changed to mut_frame to indicate it's mutable
            var frame = mut_frame;  // Create mutable copy
            // Print the result
            switch (frame.value.type) {
                .STRING => std.debug.print("String result: \"{s}\"\n", .{frame.value.data.string}),
                .BOOL => std.debug.print("Boolean result: {}\n", .{frame.value.data.boolean}),
                .INT => std.debug.print("Integer result: {}\n", .{frame.value.data.int}),
                else => std.debug.print("Other type result\n", .{}),
            }
            // Clean up the result frame
            frame.deinit();
        }
        ip = vm.ip;
    }
}


           