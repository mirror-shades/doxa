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

const Stack = struct {
    data: [STACK_SIZE]Frame,
    sp: usize = 0,

    pub fn init() Stack {
        var stack = Stack{
            .data = undefined,
            .sp = 0,
        };
        // Initialize all frames to NOTHING
        for (&stack.data) |*frame| {
            frame.* = Frame.initNothing();
        }
        return stack;
    }

    pub fn deinit(self: *Stack) void {
        var i: usize = 0;
        while (i < self.sp) : (i += 1) {
            if (self.data[i].allocator != null) {
                self.data[i].deinit();
            }
        }
        self.sp = 0;
    }

    pub fn push(self: *Stack, value: Frame) !void {
        if (self.sp >= STACK_SIZE) {
            return error.StackOverflow;
        }
        self.data[self.sp] = value;
        self.sp += 1;
    }

    pub fn pop(self: *Stack) !Frame {
        if (self.sp == 0) {
            return error.StackUnderflow;
        }
        self.sp -= 1;
        return self.data[self.sp];
    }

    pub fn peek(self: Stack) !Frame {
        if (self.sp == 0) {
            return error.StackUnderflow;
        }
        return self.data[self.sp - 1];
    }

    pub fn size(self: Stack) usize {
        return self.sp;
    }

    pub fn isEmpty(self: Stack) bool {
        return self.sp == 0;
    }

    pub fn clear(self: *Stack) void {
        self.sp = 0;
    }
};

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
    stack: Stack,  // Replace [STACK_SIZE]Frame with Stack
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
            .stack = Stack.init(),  // Now this will work
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

        // Initialize stack with NOTHING values if needed
        var i: usize = 0;
        while (i < STACK_SIZE) : (i += 1) {
            vm.stack.data[i] = Frame.initNothing();
        }
        
        // Initialize global constants
        for (constants, 0..) |constant, j| {
            vm.global_constants[j] = constant.value;
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
        // First clean up the stack
        self.stack.deinit();
        
        // Clean up variables
        for (self.variables.items) |*var_frame| {
            if (var_frame.allocator != null) {
                var_frame.deinit();
            }
        }
        self.variables.deinit();
        
        // Clean up functions
        self.functions.deinit();
        
        // Clean up frames
        self.frames.deinit();
        
        // Clean up call stack and global function
        if (self.call_stack.items.len > 0) {
            const global_frame = &self.call_stack.items[0];
            self.allocator.destroy(global_frame.function);
        }
        self.call_stack.deinit();
        
        // Finally, free global constants
        self.allocator.free(self.global_constants);
    }

    fn push(self: *VM, value: Frame) void {
        self.stack.push(value) catch {
            self.reporter.reportFatalError("Stack overflow", .{});
            return;
        };
    }

    fn pop(self: *VM) ?Frame {
        return self.stack.pop() catch {
            self.reporter.reportFatalError("Stack underflow", .{});
            return null;
        };
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

    pub fn eval(self: *VM) !?*Frame {
        while (true) {
            if (self.current_frame) |frame| {
                if (frame.ip >= frame.function.code.len) {
                    // Create a new frame with NOTHING value for empty returns
                    const result = try self.allocator.create(Frame);
                    result.* = Frame.initNothing();
                    return result;
                }
                
                const opcode: instructions.OpCode = @enumFromInt(frame.function.code[frame.ip]);
                frame.ip += 1;
                self.ip = frame.ip;

                switch (opcode) {
                    .OP_HALT => {
                        if (self.stack.size() > 0) {
                            // Get the top value from the stack
                            const top_value = try self.stack.peek();
                            
                            // Create a new Frame on the heap for the return value
                            const result = try self.allocator.create(Frame);
                            
                            // Create a deep copy if it's a string
                            if (top_value.value.type == .STRING) {
                                const str = try top_value.asString();
                                result.* = try Frame.initString(self.allocator, str);
                            } else {
                                result.* = top_value;  // Copy the value for other types
                            }
                            
                            std.debug.print("Returning value of type: {}\n", .{result.value.type});
                            return result;
                        }
                        // Only return 0 if stack is empty
                        const result = try self.allocator.create(Frame);
                        result.* = Frame.initInt(0);
                        return result;
                    },
                    .OP_CONST => {
                        const constant_index = self.read_byte();
                        const constant_value = self.get_constant(constant_index) orelse return null;
                        self.push(constant_value);
                    },
                    .OP_VAR => {
                        const var_index = self.read_byte();
                        
                        // If we're in a function call
                        if (self.current_frame) |func_frame| {
                            // Access the argument relative to the frame's base pointer
                            const arg_position = func_frame.bp + var_index;
                            if (arg_position >= self.sp) {
                                self.reporter.reportFatalError("Variable index out of bounds", .{});
                                return null;
                            }
                            self.push(self.stack.data[arg_position]);
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
                        std.debug.print("Executing OP_CONCAT\n", .{});
                        var b = self.pop() orelse return null;
                        var a = self.pop() orelse return null;
                        std.debug.print("Concatenating: ", .{});
                        debugPrintFrame(a);
                        std.debug.print(" with ", .{});
                        debugPrintFrame(b);
                        std.debug.print("\n", .{});
                        defer {
                            b.deinit();
                            a.deinit();
                        }
                        const result = try concat(self.allocator, &a, &b);
                        std.debug.print("Concat result: ", .{});
                        debugPrintFrame(result);
                        std.debug.print("\n", .{});
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
                        var a = self.pop() orelse return null;
                        defer a.deinit();
                        const result = try substr(self.allocator, &a, start, len);
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

    fn executeReturn(self: *VM) !?*Frame {
        const return_value = self.pop() orelse return error.EmptyStack;
        
        // If we're in the global frame, return the value
        if (self.call_stack.items.len <= 1) {
            // Allocate the return value on the heap
            const result = try self.allocator.create(Frame);
            result.* = return_value;
            return result;
        }
        
        // Pop current frame
        _ = self.call_stack.pop();
        self.current_frame = &self.call_stack.items[self.call_stack.items.len - 1];
        
        // Restore caller's stack pointer but keep one slot for return value
        self.sp = self.current_frame.?.bp;
        try self.stack.push(return_value);
        
        // Restore caller's instruction pointer
        self.ip = self.current_frame.?.return_addr;
        
        return null;
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

    fn concat(allocator: std.mem.Allocator, a: *Frame, b: *Frame) !Frame {
        if (a.value.type == .STRING and b.value.type == .STRING) {
            const a_str = try a.asString();
            const b_str = try b.asString();
            
            // Allocate exactly the right amount of memory
            const new_string = try allocator.alloc(u8, a_str.len + b_str.len);
            @memcpy(new_string[0..a_str.len], a_str);
            @memcpy(new_string[a_str.len..], b_str);
            
            return Frame{
                .value = .{ .type = .STRING, .data = .{ .string = new_string } },
                .allocator = allocator,
            };
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

    fn substr(allocator: std.mem.Allocator, a: *Frame, start: u8, len: u8) !Frame {
        if (a.value.type == .STRING) {
            const a_str = try a.asString();
            const start_idx = @as(usize, start);
            const length = @as(usize, len);
            
            if (start_idx >= a_str.len or start_idx + length > a_str.len) {
                return error.IndexOutOfBounds;
            }
            
            // Create a new string with the substring
            const slice = a_str[start_idx..][0..length];
            const new_string = try allocator.alloc(u8, length);
            @memcpy(new_string, slice);
            
            return Frame{
                .value = .{ .type = .STRING, .data = .{ .string = new_string } },
                .allocator = allocator,
            };
        }
        return error.TypeError;
    }

    // Helper methods for stack operations relative to current frame
    fn pushValue(self: *VM, value: Frame) !void {
        if (self.sp >= STACK_SIZE) {
            return error.StackOverflow;
        }
        self.stack.data[self.sp] = value;
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
        return self.stack.data[self.sp];
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
            const value = self.stack.data[i].value;
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

    fn debugPrintFrame(frame: Frame) void {
        switch (frame.value.type) {
            .STRING => std.debug.print("STRING: \"{s}\"", .{frame.value.data.string}),
            .INT => std.debug.print("INT: {}", .{frame.value.data.int}),
            .FLOAT => std.debug.print("FLOAT: {d}", .{frame.value.data.float}),
            .BOOL => std.debug.print("BOOL: {}", .{frame.value.data.boolean}),
            .NOTHING => std.debug.print("NOTHING", .{}),
            else => std.debug.print("OTHER", .{}),
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
        Frame.initString(allocator, "Hello") catch unreachable,    // constant[0]
        Frame.initString(allocator, " World") catch unreachable,   // constant[1]
        Frame.initString(allocator, "Hello") catch unreachable,    // constant[2]
    };
    
    // Ensure constants are cleaned up
    defer {
        for (&constants_array) |*constant| {
            constant.deinit();
        }
    }

    // Add debug prints to see what's happening
    std.debug.print("Starting VM execution\n", .{});
    
    const code = [_]u8{
        // Test concatenation: "Hello" + " World"
        @intFromEnum(instructions.OpCode.OP_CONST), 0,    // Push "Hello"
        @intFromEnum(instructions.OpCode.OP_CONST), 1,    // Push " World"
        @intFromEnum(instructions.OpCode.OP_CONCAT),      // Concatenate
        @intFromEnum(instructions.OpCode.OP_HALT),        // Stop after concatenation
    };

    std.debug.print("Code length: {}\n", .{code.len});

    var reporter = Reporting.initStderr();
    var vm = VM.init(allocator, &code, &constants_array, &reporter);
    defer vm.deinit();

    // Run the VM and handle results
    if (vm.eval()) |maybe_result| {
        if (maybe_result) |result| {
            std.debug.print("Result type: {}\n", .{result.value.type});
            // Print the result
            switch (result.value.type) {
                .STRING => std.debug.print("String result: \"{s}\"\n", .{result.value.data.string}),
                .BOOL => std.debug.print("Boolean result: {}\n", .{result.value.data.boolean}),
                .INT => std.debug.print("Integer result: {}\n", .{result.value.data.int}),
                .FLOAT => std.debug.print("Float result: {d}\n", .{result.value.data.float}),
                .NOTHING => std.debug.print("Nothing result\n", .{}),
                else => std.debug.print("Other type result\n", .{}),
            }
            
            // Clean up the result
            if (result.value.type == .STRING) {
                result.deinit();
            }
            allocator.destroy(result);
        } else {
            std.debug.print("No result returned\n", .{});
        }
    } else |err| {
        std.debug.print("Error: {}\n", .{err});
    }
}


           