const std = @import("std");
const instructions = @import("instructions.zig");
const Reporting = @import("reporting.zig").Reporting;

const Frame = struct {
    value: instructions.Value,
    allocator: ?std.mem.Allocator,
    owns_value: bool = true,
    
    // Helper functions
    pub fn initInt(x:  i32) Frame {
        return Frame{
            .value = instructions.Value{ .type = .INT, .data = .{ .int = x } },
            .allocator = null,
        };
    }

    pub fn initFloat(x: f32) Frame {
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

    pub fn initStruct(allocator: std.mem.Allocator, type_name: []const u8, num_fields: u8) !Frame {
        const struct_value = instructions.Value{
            .type = .STRUCT,
            .data = .{
                .struct_val = .{
                    .type_name = try allocator.dupe(u8, type_name),
                    .fields = std.StringHashMap(instructions.Value).init(allocator),
                    .num_fields = num_fields,
                },
            },
        };

        return Frame{
            .value = struct_value,
            .allocator = allocator,
            .owns_value = true,
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
    
    pub fn asInt(self: Frame) ! i32 {
        if (!self.typeIs(.INT)) {
            return error.TypeError;
        }
        return self.value.data.int;
    }

    pub fn asFloat(self: Frame) !f32 {
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
        if (!self.owns_value) return;
        
        if (self.allocator) |alloc| {
            switch (self.value.type) {
                .STRING => alloc.free(self.value.data.string),
                .STRUCT => {
                    // Free the type name
                    alloc.free(self.value.data.struct_val.type_name);
                    // Deinit the fields hash map
                    self.value.data.struct_val.fields.deinit();
                },
                else => {},
            }
        }
    }

    pub fn reference(value: instructions.Value) Frame {
        return Frame{
            .value = value,
            .allocator = null,
            .owns_value = false,
        };
    }
};

const STACK_SIZE = 512;
const MAX_FRAMES = 64; // Or whatever maximum call depth you want to support

const Stack = struct {
    data: [STACK_SIZE]Frame,
    sp:  u32 = 0,

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
        var i:  u32 = 0;
        while (i < self.sp) : (i += 1) {
            self.data[i].deinit();
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

    pub fn size(self: Stack)  u32 {
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
    ip:  u32,              // Instruction pointer for this frame
    bp:  u32,              // Base pointer (start of this frame's stack space)
    sp:  u32,              // Stack pointer for this frame
    function: *const instructions.Function, // Reference to the function being executed
    return_addr:  u32,     // Where to return to in the caller's code
};

pub const VM = struct {
    allocator: std.mem.Allocator,
    code: []const u8,
    stack: Stack,  // Replace [STACK_SIZE]Frame with Stack
    constants: []Frame,
    variables: std.ArrayList(Frame),
    functions: std.ArrayList(*instructions.Function),
    frames: std.ArrayList(CallFrame),
    frame_count:  u32,
    sp:  u32,
    ip:  u32,
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
        var i:  u32 = 0;
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
        // Clean up variables first
        for (self.variables.items) |*var_frame| {
            var_frame.deinit();
        }
        self.variables.deinit();
        
        // Clean up stack
        self.stack.deinit();
        
        // Clean up other resources
        self.functions.deinit();
        self.frames.deinit();
        
        if (self.call_stack.items.len > 0) {
            const global_frame = &self.call_stack.items[0];
            self.allocator.destroy(global_frame.function);
        }
        self.call_stack.deinit();
        
        self.allocator.free(self.global_constants);
    }

    fn initStruct(self: *VM, type_name: []const u8, num_fields: u8) !Frame {
        return Frame.initStruct(self.allocator, type_name, num_fields);
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
                            const top_value = try self.stack.peek();
                            
                            // Create a new Frame on the heap for the return value
                            const result = try self.allocator.create(Frame);
                            
                            // Create a deep copy if needed
                            if (top_value.value.type == .STRING) {
                                const str = try top_value.asString();
                                result.* = try Frame.initString(self.allocator, str);
                            } else if (top_value.value.type == .STRUCT) {
                                // For structs, we need to do a deep copy
                                const type_name = top_value.value.data.struct_val.type_name;
                                result.* = try self.initStruct(type_name, top_value.value.data.struct_val.num_fields);
                                
                                // Copy all fields
                                var it = top_value.value.data.struct_val.fields.iterator();
                                while (it.next()) |entry| {
                                    try result.value.data.struct_val.fields.put(entry.key_ptr.*, entry.value_ptr.*);
                                }
                            } else {
                                result.* = Frame{ 
                                    .value = top_value.value,
                                    .allocator = null,
                                    .owns_value = true,
                                };
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
                                    const a_float = @as(f32, @floatFromInt(a.asInt() catch return null));
                                    const b_float = @as(f32, @floatFromInt(b.asInt() catch return null));
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
                                    const a_float = @as(f32, @floatFromInt(a.asInt() catch return null));
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
                                    const b_float = @as(f32, @floatFromInt(b.asInt() catch return null));
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
                        const a_val: f32 = switch (a.value.type) {
                            .FLOAT => a.asFloat() catch return null,
                            .INT => @as(f32, @floatFromInt(a.asInt() catch return null)),
                            else => {
                                self.reporter.reportFatalError("Invalid type for float operation", .{});
                                return null;
                            },
                        };

                        const b_val: f32 = switch (b.value.type) {
                            .FLOAT => b.asFloat() catch return null,
                            .INT => @as(f32, @floatFromInt(b.asInt() catch return null)),
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
                        const float_val = @as(f32, @floatFromInt(int_val));
                        self.push(Frame.initFloat(float_val));
                        std.debug.print("I2F: Converted {} to {d}\n", .{int_val, float_val});
                    },
                    // f2i
                    instructions.OpCode.OP_F2I => {
                        const value = self.pop() orelse return null;
                        const float_val = value.asFloat() catch return null;
                        const int_val = @as( i32, @intFromFloat(std.math.trunc(float_val)));
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
                    instructions.OpCode.OP_STRUCT_NEW => {
                        const num_fields = self.read_byte();
                        const type_name_idx = self.read_byte();
                        std.debug.print("Creating struct with {} fields, type name at index {}\n", .{num_fields, type_name_idx});
                        const type_name_frame = self.get_constant(type_name_idx) orelse return null;
                        const type_name = type_name_frame.asString() catch {
                            self.reporter.reportFatalError("Expected string constant for struct type name", .{});
                            return null;
                        };
                        const result = try self.initStruct(type_name, num_fields);
                        self.push(result);
                    },
                    instructions.OpCode.OP_SET_FIELD => {
                        std.debug.print("Setting field - Stack size: {}\n", .{self.stack.size()});
                        const value = self.pop() orelse return null;
                        const field_name_frame = self.pop() orelse return null;
                        var struct_frame = self.pop() orelse return null;
                        
                        if (struct_frame.value.type != .STRUCT) {
                            self.reporter.reportFatalError("Expected struct for field assignment", .{});
                            return null;
                        }
                        
                        const field_name = field_name_frame.asString() catch {
                            self.reporter.reportFatalError("Expected string for field name", .{});
                            return null;
                        };
                        
                        // Set the field in the struct
                        try struct_frame.value.data.struct_val.fields.put(
                            field_name,
                            value.value
                        );
                        
                        // Push the struct back onto the stack
                        self.push(struct_frame);
                    },
                    instructions.OpCode.OP_GET_FIELD => {
                        const field_name_frame = self.pop() orelse return null;
                        const struct_frame = self.pop() orelse return null;
                        
                        if (struct_frame.value.type != .STRUCT) {
                            self.reporter.reportFatalError("Expected struct for field access", .{});
                            return null;
                        }
                        
                        const field_name = field_name_frame.asString() catch {
                            self.reporter.reportFatalError("Expected string for field name", .{});
                            return null;
                        };
                        
                        // Get the field from the struct
                        if (struct_frame.value.data.struct_val.fields.get(field_name)) |field_value| {
                            // Create a new frame for the field value
                            const result = Frame{
                                .value = field_value,
                                .allocator = null,  // Field values don't own their memory
                            };
                            self.push(result);
                        } else {
                            self.reporter.reportFatalError("Field not found in struct", .{});
                            return null;
                        }
                    },
                }

                // Your existing debug print code
                std.debug.print("Stack after operation: ", .{});
                var i:  u32 = 0;
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

    fn i2f(self: *VM, a:  i32) !void {
        const result: f32 = @as(f32, @floatFromInt(a));
        self.push(Frame.initFloat(result));
    }

    fn f2i(self: *VM, a: f32) !void {
        const result:  i32 = @intFromFloat(std.math.trunc(a));
        self.push(Frame.initInt(result));
    }

   fn i_add(a:  i32, b:  i32) !Frame {
        if (b > 0 and a > std.math.maxInt( i32) - b) {
            return error.IntegerOverflow;
        }
        if (b < 0 and a < std.math.minInt( i32) - b) {
            return error.IntegerOverflow;
        }
        return Frame.initInt(a + b);
     }   

    fn i_sub(a:  i32, b:  i32) !Frame {
        // Check for overflow: if b is positive, a must not be less than MIN + b
        // if b is negative, a must not be greater than MAX + b
        if (b > 0 and a < std.math.minInt( i32) + b) {
            return error.IntegerOverflow;
        }
        if (b < 0 and a > std.math.maxInt( i32) + b) {
            return error.IntegerOverflow;
        }
        return Frame.initInt(a - b);
    }

    fn i_mul(a:  i32, b:  i32) !Frame {
        // Special cases first
        if (a == 0 or b == 0) return Frame.initInt(0);
        if (a == 1) return Frame.initInt(b);
        if (b == 1) return Frame.initInt(a);

        // Check for overflow
        if (a > 0) {
            if (b > 0) {
                if (a > @divTrunc(std.math.maxInt( i32), b)) return error.IntegerOverflow;
            } else {
                if (b < @divTrunc(std.math.minInt( i32), a)) return error.IntegerOverflow;
            }
        } else {
            if (b > 0) {
                if (a < @divTrunc(std.math.minInt( i32), b)) return error.IntegerOverflow;
            } else {
                if (a != 0 and b < @divTrunc(std.math.maxInt( i32), a)) return error.IntegerOverflow;
            }
        }
        return Frame.initInt(a * b);
    }

    fn f_add(a: f32, b: f32) !Frame {
        if (a + b < -std.math.floatMax(f32) or a + b > std.math.floatMax(f32)) {
            return error.FloatOverflow;
        }
        return Frame.initFloat(a + b);
    }

    fn f_sub(a: f32, b: f32) !Frame {
        if (a - b < -std.math.floatMax(f32) or a - b > std.math.floatMax(f32)) {
            return error.FloatOverflow;
        }
        return Frame.initFloat(a - b);
    }

    fn f_mul(a: f32, b: f32) !Frame {
        if (a * b < -std.math.floatMax(f32) or a * b > std.math.floatMax(f32)) {
            return error.FloatOverflow;
        }
        return Frame.initFloat(a * b);
    }

    fn f_div(a: f32, b: f32) !Frame {
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
            const a_val: f32 = if (a.value.type == .INT)
                @as(f32, @floatFromInt(try a.asInt()))
            else
                try a.asFloat();

            const b_val: f32 = if (b.value.type == .INT)
                @as(f32, @floatFromInt(try b.asInt()))
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
            const a_val: f32 = if (a.value.type == .INT)
                @as(f32, @floatFromInt(try a.asInt()))
            else
                try a.asFloat();

            const b_val: f32 = if (b.value.type == .INT)
                @as(f32, @floatFromInt(try b.asInt()))
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
            const a_val: f32 = if (a.value.type == .INT)
                @as(f32, @floatFromInt(try a.asInt()))
            else
                try a.asFloat();

            const b_val: f32 = if (b.value.type == .INT)
                @as(f32, @floatFromInt(try b.asInt()))
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
            return Frame.initInt(@as( i32, @intCast(a_str.len)));
        }
        return error.TypeError;
    }

    fn substr(allocator: std.mem.Allocator, a: *Frame, start: u8, len: u8) !Frame {
        if (a.value.type == .STRING) {
            const a_str = try a.asString();
            const start_idx = @as( u32, start);
            const length = @as( u32, len);
            
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
        var i:  u32 = 0;
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

    // Create constants including struct type name
    var constants_array = [_]Frame{
        try Frame.initString(allocator, "Person"),    // constant[0] - struct type name
        try Frame.initString(allocator, "name"),      // constant[1] - field name
        try Frame.initString(allocator, "John"),      // constant[2] - field value
    };
    
    defer {
        for (&constants_array) |*constant| {
            constant.deinit();
        }
    }

    // Create bytecode that:
    // 1. Creates a new Person struct
    // 2. Sets the name field
    // 3. Halts
    const code = [_]u8{
        @intFromEnum(instructions.OpCode.OP_CONST), 0,         // Push "Person" type name
        @intFromEnum(instructions.OpCode.OP_STRUCT_NEW), 1, 0, // Create new struct with 1 field, using constant 0
        @intFromEnum(instructions.OpCode.OP_CONST), 1,         // Push "name" field name
        @intFromEnum(instructions.OpCode.OP_CONST), 2,         // Push "John" field value
        @intFromEnum(instructions.OpCode.OP_SET_FIELD),        // Set the field
        @intFromEnum(instructions.OpCode.OP_HALT),            // Halt with struct on stack
    };

    std.debug.print("Starting VM execution with struct test\n", .{});

    var reporter = Reporting.initStderr();
    var vm = VM.init(allocator, &code, &constants_array, &reporter);
    defer vm.deinit();

    // Add debug prints for VM state
    std.debug.print("Initial VM state:\n", .{});
    std.debug.print("Code length: {}\n", .{vm.code.len});
    std.debug.print("Constants length: {}\n", .{vm.constants.len});

    // Run the VM and handle results
    if (vm.eval()) |maybe_result| {
        if (maybe_result) |result| {
            std.debug.print("Result type: {}\n", .{result.value.type});
            
            // Print the struct contents
            if (result.value.type == .STRUCT) {
                const struct_val = result.value.data.struct_val;
                std.debug.print("Struct type: {s}\n", .{struct_val.type_name});
                std.debug.print("Number of fields: {}\n", .{struct_val.num_fields});
                
                // Print all fields
                var it = struct_val.fields.iterator();
                while (it.next()) |entry| {
                    std.debug.print("Field {s}: ", .{entry.key_ptr.*});
                    switch (entry.value_ptr.*.type) {
                        .STRING => std.debug.print("{s}\n", .{entry.value_ptr.*.data.string}),
                        .INT => std.debug.print("{}\n", .{entry.value_ptr.*.data.int}),
                        .FLOAT => std.debug.print("{d}\n", .{entry.value_ptr.*.data.float}),
                        .BOOL => std.debug.print("{}\n", .{entry.value_ptr.*.data.boolean}),
                        else => std.debug.print("<other type>\n", .{}),
                    }
                }
            }
            
            // Clean up the result
            result.deinit();
            allocator.destroy(result);
        } else {
            std.debug.print("No result returned\n", .{});
        }
    } else |err| {
        std.debug.print("Error: {}\n", .{err});
    }
}
