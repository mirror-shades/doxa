const std = @import("std");
const instructions = @import("instructions.zig");
const Reporting = @import("reporting.zig").Reporting;

const STACK_SIZE: u16 = 1024;
const MAX_FRAMES: u16 = 1024;

const Frame = struct {
    value: instructions.Value,
    allocator: ?std.mem.Allocator,
    owns_value: bool = true,
    
    // Helper functions
    pub fn initInt(x:  i32) Frame {
        return Frame{
            .value = instructions.Value{ .type = .INT, .nothing = false, .data = .{ .int = x } },
            .allocator = null,
        };
    }

    pub fn initFloat(x: f32) Frame {
        return Frame{
            .value = instructions.Value{ .type = .FLOAT, .nothing = false, .data = .{ .float = x } },
            .allocator = null,
        };
    }

    pub fn initString(interner: *StringInterner, x: []const u8) !Frame {
        const interned = try interner.intern(x);
        return Frame{
            .value = instructions.Value{ .type = .STRING, .nothing = false, .data = .{ .string = interned } },
            .allocator = null,  // No allocator needed since string is interned
            .owns_value = false,  // Interned strings are owned by the interner
        };
    }

    pub fn initBoolean(x: bool) Frame {
        return Frame{
            .value = instructions.Value{ .type = .BOOL, .nothing = false, .data = .{ .boolean = x } },
            .allocator = null,
        };
    }

    pub fn initStruct(allocator: std.mem.Allocator, type_name: []const u8, num_fields: u8) !Frame {
        const struct_ptr = try allocator.create(instructions.StructValue);
        struct_ptr.* = .{
            .type_name = try allocator.dupe(u8, type_name),
            .fields = std.StringHashMap(instructions.Value).init(allocator),
            .num_fields = num_fields,
        };

        return Frame{
            .value = instructions.Value{
                .type = .STRUCT,
                .nothing = false,
                .data = .{ .struct_val = struct_ptr },
            },
            .allocator = allocator,
            .owns_value = true,
        };
    }

    pub fn initNothing(varType: instructions.ValueType) Frame {
        switch (varType) {
            .INT => return Frame{
                .value = instructions.Value{ .type = .INT, .nothing = true, .data = .{ .int = 0 } },
                .allocator = null,
            },
            .FLOAT => return Frame{
                .value = instructions.Value{ .type = .FLOAT, .nothing = true, .data = .{ .float = 0.0 } },
                .allocator = null,
            },
            .BOOL => return Frame{
                .value = instructions.Value{ .type = .BOOL, .nothing = true, .data = .{ .boolean = false } },
                .allocator = null,
            },
            .STRING => return Frame{
                .value = instructions.Value{ .type = .STRING, .nothing = true, .data = .{ .string = "" } },
                .allocator = null,
            },
            else => std.debug.panic("Invalid type for initNothing", .{}),
        }
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

    pub fn asStruct(self: Frame) !*instructions.StructValue {
        if (!self.typeIs(.STRUCT)) {
            return error.TypeError;
        }
        return self.value.data.struct_val;
    }

    pub fn asArray(self: Frame) !*instructions.ArrayValue {
        if (!self.typeIs(.ARRAY)) {
            return error.TypeError;
        }
        return self.value.data.array_val;
    }

    pub fn deinit(self: Frame) void {
        if (!self.owns_value) return;
        
        if (self.allocator) |alloc| {
            switch (self.value.type) {
                .STRING => alloc.free(self.value.data.string),
                .STRUCT => {
                    alloc.free(self.value.data.struct_val.type_name);
                    var fields = self.value.data.struct_val.fields;
                    fields.deinit();
                    alloc.destroy(self.value.data.struct_val);  // Free the struct itself
                },
                .ARRAY => {
                    var items = self.value.data.array_val.items;
                    items.deinit();
                    alloc.destroy(self.value.data.array_val);
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

    pub fn initArray(allocator: std.mem.Allocator, size: u8) !Frame {
        const array = try allocator.create(instructions.ArrayValue);
        array.* = instructions.ArrayValue{
            .items = std.ArrayList(instructions.Value).init(allocator),
            .capacity = size,
        };
        
        return Frame{
            .value = instructions.Value{
                .type = .ARRAY,
                .nothing = false,
                .data = .{ .array_val = array },
            },
            .allocator = allocator,
            .owns_value = true,  // We own this array
        };
    }

    pub fn initValue(value: instructions.Value) Frame {
        return Frame{
            .value = value,
            .allocator = null,
            .owns_value = false,
        };
    }

    pub fn isEnum(self: Frame) bool {
        return self.value.type == .ENUM;
    }

    pub fn asEnum(self: Frame) !u8 {
        if (self.value.type != .ENUM) {
            return error.TypeError;
        }
        return self.value.data.enum_val.variant;
    }
};

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
            frame.* = Frame.initNothing(.INT);
        }
        return stack;
    }

    pub fn deinit(self: *Stack) void {
        var i: u32 = 0;
        while (i < self.sp) : (i += 1) {
            var frame = self.data[i];
            frame.deinit();
        }
        self.sp = 0;
    }

    pub fn push(self: *Stack, value: Frame) !void {
        if (self.sp >= STACK_SIZE) {
            return error.StackOverflow;
        }
        self.data[self.sp] = value;
        self.sp += 1; 
        
        // More detailed debug output
        switch (value.value.type) {
            .INT => std.debug.print("Stack push: INT({}) sp={}\n", .{value.value.data.int, self.sp}),
            .FLOAT => std.debug.print("Stack push: FLOAT({d}) sp={}\n", .{value.value.data.float, self.sp}),
            else => std.debug.print("Stack push: type={}, sp={}\n", .{value.value.type, self.sp}),
        }
    }

    pub fn pop(self: *Stack) !Frame {
        if (self.sp == 0) {
            return error.StackUnderflow;
        }
        self.sp -= 1;
        const value = self.data[self.sp];
        
        // More detailed debug output
        switch (value.value.type) {
            .INT => std.debug.print("Stack pop: INT({}) sp={}\n", .{value.value.data.int, self.sp}),
            .FLOAT => std.debug.print("Stack pop: FLOAT({d}) sp={}\n", .{value.value.data.float, self.sp}),
            else => std.debug.print("Stack pop: type={}, sp={}\n", .{value.value.type, self.sp}),
        }
        
        return value;
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

const StringInterner = struct {
    strings: std.StringHashMap([]const u8),
    allocator: std.mem.Allocator,
    
    pub fn init(allocator: std.mem.Allocator) StringInterner {
        return StringInterner{
            .strings = std.StringHashMap([]const u8).init(allocator),
            .allocator = allocator,
        };
    }

    pub fn deinit(self: *StringInterner) void {
        var it = self.strings.iterator();
        while (it.next()) |entry| {
            self.allocator.free(entry.key_ptr.*);
        }
        self.strings.deinit();
    }
    
    pub fn intern(self: *StringInterner, string: []const u8) ![]const u8 {
        if (self.strings.get(string)) |existing| {
            return existing;
        }
        const copy = try self.allocator.dupe(u8, string);
        try self.strings.put(copy, copy);
        return copy;
    }
};

const ArrayInterner = struct {
    arrays: std.AutoHashMap(u64, *instructions.ArrayValue),
    allocator: std.mem.Allocator,
    
    pub fn init(allocator: std.mem.Allocator) ArrayInterner {
        return ArrayInterner{
            .arrays = std.AutoHashMap(u64, *instructions.ArrayValue).init(allocator),
            .allocator = allocator,
        };
    }

    pub fn deinit(self: *ArrayInterner) void {
        var it = self.arrays.iterator();
        while (it.next()) |entry| {
            entry.value_ptr.*.items.deinit();
            self.allocator.destroy(entry.value_ptr.*);
        }
        self.arrays.deinit();
    }

    fn hashArray(array: *instructions.ArrayValue) u64 {
        var hasher = std.hash.Wyhash.init(0);
        
        // Hash the capacity and length
        hasher.update(std.mem.asBytes(&array.capacity));
        const len = array.items.items.len;
        hasher.update(std.mem.asBytes(&len));
        
        // Hash each item in the array, including its position
        for (array.items.items, 0..) |item, i| {
            // Hash the position first
            hasher.update(std.mem.asBytes(&i));
            
            // Then hash the type
            hasher.update(std.mem.asBytes(&item.type));
            
            // Then hash the value based on type
            switch (item.type) {
                .INT => {
                    hasher.update(std.mem.asBytes(&item.data.int));
                },
                .FLOAT => {
                    const float_bits = @as(u32, @bitCast(item.data.float));
                    hasher.update(std.mem.asBytes(&float_bits));
                },
                .BOOL => {
                    hasher.update(std.mem.asBytes(&item.data.boolean));
                },
                .STRING => {
                    hasher.update(item.data.string);
                },
                .ARRAY => {
                    // For nested arrays, just hash their length
                    const nested_len = item.data.array_val.items.items.len;
                    hasher.update(std.mem.asBytes(&nested_len));
                },
                else => {
                    // For other types, hash their memory representation
                    hasher.update(std.mem.asBytes(&item));
                },
            }
        }
        
        return hasher.final();
    }
    
    pub fn intern(self: *ArrayInterner, array: *instructions.ArrayValue) !*instructions.ArrayValue {
        const hash = hashArray(array);
        
        // Check if we already have this array
        if (self.arrays.get(hash)) |existing| {
            array.items.deinit();
            self.allocator.destroy(array);
            return existing;
        }
        
        // Store the new array
        try self.arrays.put(hash, array);
        return array;
    }
};

const CallFrame = struct {
    ip:  u32,              // Instruction pointer for this frame
    bp:  u32,              // Base pointer (start of this frame's stack space)
    sp:  u32,              // Stack pointer for this frame
    function: *const instructions.Function, // Reference to the function being executed
    return_addr:  u32,     // Where to return to in the caller's code
};

const BlockScope = struct {
    start_ip: u32,
    end_ip: u32,
    base_sp: u32,
    var_start: u32,
    var_count: u32,
    parent: ?*BlockScope,
    
    pub fn init(start: u32, base_sp: u32, var_start: u32, parent: ?*BlockScope) BlockScope {
        std.debug.print("Creating new block scope: var_start={}\n", .{var_start});
        return BlockScope{
            .start_ip = start,
            .end_ip = undefined,
            .base_sp = base_sp,
            .var_start = var_start,
            .var_count = 0,
            .parent = parent,
        };
    }
    
    pub fn cleanup(self: BlockScope, vm: *VM) void {
        std.debug.print("Cleaning up block scope: var_start={}, var_count={}\n", .{
            self.var_start, self.var_count
        });
        
        // Clean up variables in this scope
        var i: u32 = 0;
        while (i < self.var_count) : (i += 1) {
            const var_idx = self.var_start + i;
            if (var_idx < vm.current_scope_vars.items.len) {
                vm.current_scope_vars.items[var_idx].deinit();
                // Mark the variable as inaccessible
                vm.current_scope_vars.items[var_idx] = Frame.initNothing(.INT);
            }
        }
        // Reset stack pointer
        vm.sp = self.base_sp;
    }
};

pub const VM = struct {
    allocator: std.mem.Allocator,
    code: []const u8,
    stack: Stack,  // Replace [STACK_SIZE]Frame with Stack
    constants: []Frame,
    variables: std.ArrayList(Frame),
    functions: std.ArrayList(*instructions.Function),
    frames: std.ArrayList(CallFrame),
    frame_count: u32,
    sp: u32,
    ip:  u32,
    reporter: *Reporting,
    call_stack: std.ArrayList(CallFrame),
    current_frame: ?*CallFrame,  // Points to the active frame
    global_constants: []instructions.Value,  // Add this field to track global constants
    string_interner: StringInterner,
    array_interner: ArrayInterner,
    global_function: ?*instructions.Function,
    block_stack: std.ArrayList(BlockScope),
    current_scope_vars: std.ArrayList(Frame),
    match_value: ?Frame = null,

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
            .string_interner = StringInterner.init(allocator),
            .array_interner = ArrayInterner.init(allocator),
            .global_function = null,
            .block_stack = std.ArrayList(BlockScope).init(allocator),
            .current_scope_vars = std.ArrayList(Frame).init(allocator),
            .match_value = null,
        };

        // Initialize stack with NOTHING values if needed
        var i:  u32 = 0;
        while (i < STACK_SIZE) : (i += 1) {
            vm.stack.data[i] = Frame.initNothing(.INT);
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

        // Store the global function pointer
        vm.global_function = global_function;

        // Initialize the global scope with some capacity
        vm.current_scope_vars.ensureTotalCapacity(16) catch unreachable;
        
        return vm;
    }

    pub fn deinit(self: *VM) void {
        // Add cleanup for current scope variables
        for (self.current_scope_vars.items) |*var_frame| {
            var_frame.deinit();
        }
        self.current_scope_vars.deinit();
        
        // Clean up variables
        for (self.variables.items) |*var_frame| {
            var_frame.deinit();
        }
        self.variables.deinit();
        
        // Clean up stack
        var i: u32 = 0;
        while (i < self.stack.sp) : (i += 1) {
            self.stack.data[i].deinit();
        }
        
        // Clean up other resources
        self.functions.deinit();
        self.frames.deinit();
        self.call_stack.deinit();
        self.string_interner.deinit();
        self.array_interner.deinit();
        self.block_stack.deinit();
        self.allocator.free(self.global_constants);
        
        if (self.global_function) |func| {
            self.allocator.destroy(func);
        }
    }

    fn pop(self: *VM) ?Frame {
        return self.stack.pop() catch {
            self.reporter.reportFatalError("Stack underflow", .{});
            return null;
        };
    }

    fn printStack(self: *VM) void {
        std.debug.print("Stack contents ({} items):\n", .{self.stack.sp});
        var i: u32 = 0;
        while (i < self.stack.sp) : (i += 1) {
            const value = self.stack.data[i].value;
            if (value.nothing) {    
                std.debug.print("  [{}] NOTHING\n", .{i});
            } else {
                switch (value.type) {
                    .INT => std.debug.print("  [{}] INT: {}\n", .{i, value.data.int}),
                    .FLOAT => std.debug.print("  [{}] FLOAT: {}\n", .{i, value.data.float}),
                    .BOOL => std.debug.print("  [{}] BOOL: {}\n", .{i, value.data.boolean}),
                    .STRING => std.debug.print("  [{}] STRING: {s}\n", .{i, value.data.string}),
                    .ARRAY => std.debug.print("  [{}] ARRAY (len={})\n", .{i, value.data.array_val.items.items.len}),
                    else => std.debug.print("  [{}] OTHER\n", .{i}),
                }
            }   
        }
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

    fn read_byte(self: *VM) u8 {
        if (self.current_frame) |frame| {
            const byte = frame.function.code[frame.ip];
            frame.ip += 1;
            self.ip = frame.ip;  // Keep global IP in sync
            return byte;
        }
        @panic("No active frame");
    }

    fn isVariableInScope(self: *VM, var_index: u32) bool {
        std.debug.print("Checking scope for var[{}]\n", .{var_index});
        
        // Start from current block and work up through parent blocks
        var current_block = if (self.block_stack.items.len > 0) 
            &self.block_stack.items[self.block_stack.items.len - 1] 
        else 
            null;
            
        while (current_block) |block| {
            std.debug.print("  Checking block: var_start={}, var_count={}\n", 
                .{block.var_start, block.var_count});
                
            // Check if variable is defined in this block
            if (var_index >= block.var_start and 
                var_index < block.var_start + block.var_count) {
                return true;
            }
            current_block = block.parent;
        }
        
        // Check global scope
        const in_global = var_index < self.current_scope_vars.items.len;
        std.debug.print("  In global scope: {}\n", .{in_global});
        return in_global;
    }

    // remove to put in std lib as soon as possible
    fn printValue(value: instructions.Value) void {
        switch (value.type) {
            .INT => std.debug.print("{}", .{value.data.int}),
            .FLOAT => std.debug.print("{d}", .{value.data.float}),
            .BOOL => std.debug.print("{}", .{value.data.boolean}),
            .STRING => std.debug.print("\"{s}\"", .{value.data.string}),
            .ARRAY => {
                std.debug.print("[", .{});
                for (value.data.array_val.items.items, 0..) |item, i| {
                    printValue(item);
                    if (i < value.data.array_val.items.items.len - 1) {
                        std.debug.print(", ", .{});
                    }
                }
                std.debug.print("]", .{});
            },
            .STRUCT => {
                std.debug.print("{{ ", .{});
                var it = value.data.struct_val.fields.iterator();
                var first = true;
                while (it.next()) |entry| {
                    if (!first) {
                        std.debug.print(", ", .{});
                    }
                    first = false;
                    std.debug.print("{s}: ", .{entry.key_ptr.*});
                    printValue(entry.value_ptr.*);
                }
                std.debug.print(" }}", .{});
            },
            else => std.debug.print("???", .{}),
        }
    }

    pub fn eval(self: *VM) !?*Frame {
        while (true) {
            if (self.current_frame) |frame| {
                if (frame.ip >= frame.function.code.len) {
                    // Create a new frame with NOTHING value for empty returns
                    const result = try self.allocator.create(Frame);
                    result.* = Frame.initNothing(.INT);
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
                                result.* = try Frame.initString(&self.string_interner, str);
                            } else if (top_value.value.type == .STRUCT) {
                                // For structs, we need to do a deep copy
                                const type_name = top_value.value.data.struct_val.type_name;
                                result.* = try Frame.initStruct(self.allocator, type_name, top_value.value.data.struct_val.num_fields);
                                
                                // Copy all fields
                                var it = top_value.value.data.struct_val.fields.iterator();
                                while (it.next()) |entry| {
                                    try result.value.data.struct_val.fields.put(entry.key_ptr.*, entry.value_ptr.*);
                                }
                            } else if (top_value.value.type == .ARRAY) {
                                // For arrays, we need to do a deep copy
                                const array = top_value.value.data.array_val;
                                result.* = try Frame.initArray(self.allocator, @intCast(array.items.items.len));
                                try result.value.data.array_val.items.appendSlice(array.items.items);
                            } else {
                                result.* = Frame{ 
                                    .value = top_value.value,
                                    .allocator = null,
                                    .owns_value = true,
                                };
                            }
                            
                            // Clean up the stack
                            self.stack.deinit();
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
                        self.stack.push(constant_value) catch return null;
                    },
                    .OP_VAR => {
                        const var_index = self.read_byte();
                        std.debug.print("Accessing var[{}]\n", .{var_index});
                        
                        if (!self.isVariableInScope(var_index)) {
                            self.reporter.reportFatalError("Variable access out of scope", .{});
                            return null;
                        }
                        
                        if (var_index >= self.current_scope_vars.items.len) {
                            self.reporter.reportFatalError("Variable index out of bounds", .{});
                            return null;
                        }
                        
                        const var_value = self.current_scope_vars.items[var_index];
                        if (var_value.value.nothing) {
                            self.reporter.reportFatalError("Accessing cleaned up variable", .{});
                            return null;
                        }
                        
                        try self.stack.push(Frame.reference(var_value.value));
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
                            self.stack.push(result) catch return null;
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
                                    self.stack.push(result) catch return null;
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
                                    self.stack.push(result) catch return null;
                                },
                                .FLOAT => {
                                    const result = switch (opcode) {
                                        .OP_IADD => f_add(a.asFloat() catch return null, b.asFloat() catch return null),
                                        .OP_ISUB => f_sub(a.asFloat() catch return null, b.asFloat() catch return null),
                                        .OP_IMUL => f_mul(a.asFloat() catch return null, b.asFloat() catch return null),
                                        .OP_IDIV => f_div(a.asFloat() catch return null, b.asFloat() catch return null),
                                        else => unreachable,
                                    } catch return null;
                                    self.stack.push(result) catch return null;
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
                        self.stack.push(result) catch return null;
                    },  
                    // greater
                    instructions.OpCode.OP_GREATER => {
                        const b = self.pop().?;
                        const a = self.pop().?;
                        const result = greater(a, b) catch return null;
                        self.stack.push(result) catch return null;
                    },
                    // less
                    instructions.OpCode.OP_LESS => {
                        const b = self.pop().?;
                        const a = self.pop().?;
                        const result = less(a, b) catch return null;
                        self.stack.push(result) catch return null;
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
                        self.stack.push(Frame.initFloat(float_val)) catch return null;
                        std.debug.print("I2F: Converted {} to {d}\n", .{int_val, float_val});
                    },
                    // f2i
                    instructions.OpCode.OP_F2I => {
                        const value = self.pop() orelse return null;
                        const float_val = value.asFloat() catch return null;
                        const int_val = @as( i32, @intFromFloat(std.math.trunc(float_val)));
                        self.stack.push(Frame.initInt(int_val)) catch return null;
                    },
                    // equal
                    instructions.OpCode.OP_EQUAL => {
                        const b = self.pop().?;
                        const a = self.pop().?;
                        const result = equal(a, b) catch return null;
                        self.stack.push(result) catch return null;
                    },
                    // notequal
                    instructions.OpCode.OP_NOTEQUAL => {
                        const b = self.pop().?;
                        const a = self.pop().?;
                        const result = notEqual(a, b) catch return null;
                        self.stack.push(result) catch return null;
                    },
                    // set variable
                    instructions.OpCode.OP_SET_VAR => {
                        const var_index = self.read_byte();
                        const value = self.pop() orelse return null;
                        
                        // If we're in a block, update its var count
                        if (self.block_stack.items.len > 0) {
                            var current_block = &self.block_stack.items[self.block_stack.items.len - 1];
                            if (var_index >= current_block.var_start) {
                                const local_index = var_index - current_block.var_start;
                                if (local_index >= current_block.var_count) {
                                    current_block.var_count = local_index + 1;
                                }
                            }
                        }
                        
                        // Grow the current scope's variable array if needed
                        while (var_index >= self.current_scope_vars.items.len) {
                            try self.current_scope_vars.append(Frame.initNothing(.INT));
                        }
                        
                        // Clean up any existing value
                        if (var_index < self.current_scope_vars.items.len) {
                            self.current_scope_vars.items[var_index].deinit();
                        }
                        
                        // Store the new value
                        self.current_scope_vars.items[var_index] = value;
                    },
                    instructions.OpCode.OP_JUMP => {
                        const jump_offset = self.read_byte();
                        // Preserve the top stack value (result of if-block)
                        const value = try self.stack.pop();
                        
                        self.current_frame.?.ip += jump_offset;
                        self.ip = self.current_frame.?.ip;
                        
                        // Push the value back
                        try self.stack.push(value);
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
                        const jump_offset = self.read_byte();
                        const condition = self.pop() orelse return null;
                        const bool_value = condition.asBoolean() catch return null;
                        std.debug.print("JUMP_IF_FALSE: Condition={}, offset={}\n", .{bool_value, jump_offset});
                        
                        if (!bool_value) {
                            // When jumping to else block, skip the if-block code
                            self.current_frame.?.ip += jump_offset;
                            self.ip = self.current_frame.?.ip;
                            
                            // Load var[0] for the else block
                            if (self.current_scope_vars.items.len > 0) {
                                const var_value = self.current_scope_vars.items[0];
                                try self.stack.push(Frame.reference(var_value.value));
                            }
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
                    instructions.OpCode.OP_STR_CONCAT => {
                        std.debug.print("Executing OP_STR_CONCAT\n", .{});
                        var b = self.pop() orelse return null;
                        var a = self.pop() orelse return null;
                        std.debug.print("String concatenating: ", .{});
                        std.debug.print(" with ", .{});
                        std.debug.print("\n", .{});
                        defer {
                            b.deinit();
                            a.deinit();
                        }
                        const result = try self.concatStrings(&a, &b);
                        std.debug.print("concatStrings result: ", .{});
                        std.debug.print("\n", .{});
                        self.stack.push(result) catch return null;
                    },
                    instructions.OpCode.OP_STR_EQ => {
                        const b = self.pop().?;
                        const a = self.pop().?;
                        const result = str_eq(a, b) catch return null;
                        self.stack.push(result) catch return null;
                    },
                    instructions.OpCode.OP_STR_LEN => {
                        const a = self.pop().?;
                        const result = str_len(a) catch return null;
                        self.stack.push(result) catch return null;
                    },
                    instructions.OpCode.OP_SUBSTR => {
                        const start = self.read_byte();
                        const len = self.read_byte();
                        var a = self.pop() orelse return null;
                            defer a.deinit();
                        const result = try self.substr(&a, start, len);
                        self.stack.push(result) catch return null;
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
                        const result = try Frame.initStruct(self.allocator, type_name, num_fields);
                        self.stack.push(result) catch return null;
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
                        self.stack.push(struct_frame) catch return null;
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
                            self.stack.push(result) catch return null;
                        } else {
                            self.reporter.reportFatalError("Field not found in struct", .{});
                            return null;
                        }
                    },
                    instructions.OpCode.OP_ARRAY_NEW => {
                        const size = self.read_byte();
                        std.debug.print("Creating new array with capacity {}\n", .{size});
                        const array_frame = try Frame.initArray(self.allocator, size);  // Use interning for new arrays
                        try self.stack.push(array_frame);
                    },
                    instructions.OpCode.OP_ARRAY_PUSH => {
                        const value = self.pop() orelse return null;
                        var array_frame = self.pop() orelse return null;
                        
                        if (array_frame.value.type != .ARRAY) {
                            self.reporter.reportFatalError("Expected array for push operation", .{});
                            return null;
                        }
                        
                        std.debug.print("Pushing value of type {} onto array\n", .{value.value.type});
                        try array_frame.value.data.array_val.items.append(value.value);
                        try self.stack.push(array_frame);  // Push the array back onto the stack
                    },
                    instructions.OpCode.OP_ARRAY_LEN => {
                        const array_frame = self.pop() orelse return null;
                        
                        if (array_frame.value.type != .ARRAY) {
                            self.reporter.reportFatalError("Expected array for length operation", .{});
                            return null;
                        }
                        
                        const len = @as(i32, @intCast(array_frame.value.data.array_val.items.items.len));
                        std.debug.print("Array length is {}\n", .{len});
                        
                        // Push back just the array (we don't need the length for the next operation)
                        try self.stack.push(array_frame);
                        try self.stack.push(Frame.initInt(len)); // Push the length of the array back onto the stack
                    },
                    instructions.OpCode.OP_ARRAY_GET => {
                        std.debug.print("\n=== OP_ARRAY_GET ===\n", .{});
                        self.printStack();
                        
                        const index = try self.stack.pop();
                        const array_frame = try self.stack.pop();
                        
                        if (array_frame.value.type != .ARRAY) {
                            std.debug.print("TypeError: Expected array but got {}\n", .{array_frame.value.type});
                            return error.TypeError;
                        }
                        if (index.value.type != .INT) {
                            std.debug.print("TypeError: Expected integer index but got {}\n", .{index.value.type});
                            return error.TypeError;
                        }
                        
                        const array = array_frame.value.data.array_val;
                        const idx = @as(usize, @intCast(index.value.data.int));
                        
                        if (idx >= array.items.items.len) {
                            return error.IndexOutOfBounds;
                        }
                        
                        const value = array.items.items[idx];
                        try self.stack.push(array_frame);  // Push array back first
                        try self.stack.push(Frame.initValue(value));  // Then push the value
                        
                        std.debug.print("\nAfter OP_ARRAY_GET:\n", .{});
                        self.printStack();
                    },
                    instructions.OpCode.OP_ARRAY_SET => {
                        std.debug.print("\n=== OP_ARRAY_SET ===\n", .{});
                        self.printStack();
                        
                        const new_value = try self.stack.pop();
                        const index = try self.stack.pop();
                        const array_frame = try self.stack.pop();
                        
                        if (array_frame.value.type != .ARRAY) {
                            std.debug.print("TypeError: Expected array but got {}\n", .{array_frame.value.type});
                            return error.TypeError;
                        }
                        if (index.value.type != .INT) {
                            std.debug.print("TypeError: Expected integer index but got {}\n", .{index.value.type});
                            return error.TypeError;
                        }
                        
                        const array = array_frame.value.data.array_val;
                        const idx = @as(usize, @intCast(index.value.data.int));
                        
                        if (idx >= array.items.items.len) {
                            return error.IndexOutOfBounds;
                        }
                        
                        array.items.items[idx] = new_value.value;
                        try self.stack.push(array_frame);  // Put the array back on the stack
                        
                        std.debug.print("\nAfter OP_ARRAY_SET:\n", .{});
                        self.printStack();
                    },
                    instructions.OpCode.OP_ARRAY_CONCAT => {
                        std.debug.print("\n=== Executing OP_ARRAY_CONCAT ===\n", .{});
                        var b = self.pop() orelse return null;
                        var a = self.pop() orelse return null;
                        std.debug.print("Popped arrays from stack, concatenating...\n", .{});
                        const result = try self.concatArrays(&a, &b);
                        std.debug.print("Concatenation complete, pushing result\n", .{});
                        self.stack.push(result) catch return null;
                    },
                    instructions.OpCode.OP_ARRAY_SLICE => {
                        const start = self.read_byte();
                        const end = self.read_byte();
                        var array_frame = self.pop() orelse return null;
                        const result = try self.sliceArray(&array_frame, start, end);
                        self.stack.push(result) catch return null;
                    },
                    instructions.OpCode.OP_DUP => {
                        const value = try self.stack.peek();
                        try self.stack.push(Frame.reference(value.value));
                    },
                    instructions.OpCode.OP_POP => {
                        _ = try self.stack.pop();
                    },
                    instructions.OpCode.OP_BEGIN_BLOCK => {
                        const current_vars_count = self.current_scope_vars.items.len;
                        std.debug.print("BEGIN BLOCK: current vars={}\n", .{current_vars_count});
                        
                        try self.block_stack.append(BlockScope.init(
                            self.ip,
                            self.sp,
                            @intCast(current_vars_count),
                            if (self.block_stack.items.len > 0) 
                                &self.block_stack.items[self.block_stack.items.len - 1] 
                            else 
                                null
                        ));
                    },
                    instructions.OpCode.OP_END_BLOCK => {
                        if (self.block_stack.items.len == 0) {
                            self.reporter.reportFatalError("No block to end", .{});
                            return null;
                        }
                        
                        var block = self.block_stack.pop();
                        std.debug.print("END BLOCK: cleaning up {} variables starting at {}\n", 
                            .{block.var_count, block.var_start});
                        block.cleanup(self);
                    },
                    instructions.OpCode.OP_AND => {
                        const b = self.pop() orelse return null;
                        const a = self.pop() orelse return null;
                        const result = try self.logical_and(a, b);
                        self.stack.push(result) catch return null;
                    },
                    instructions.OpCode.OP_OR => {
                        const b = self.pop() orelse return null;
                        const a = self.pop() orelse return null;
                        const result = try self.logical_or(a, b);
                        self.stack.push(result) catch return null;
                    },
                    instructions.OpCode.OP_NOT => {
                        const a = self.pop() orelse return null;
                        const result = try self.logical_not(a);
                        self.stack.push(result) catch return null;
                    },
                    instructions.OpCode.OP_MATCH => {   
                        try self.executeMatch();
                    },
                    instructions.OpCode.OP_MATCH_ARM => {
                        try self.executeMatchArm();
                    },
                    instructions.OpCode.OP_MATCH_END => {
                        try self.executeMatchEnd();
                    },
                    instructions.OpCode.OP_TRY => {
                        try self.executeTry();
                    },
                    instructions.OpCode.OP_CATCH => {
                        try self.executeCatch();
                    },
                    instructions.OpCode.OP_THROW => {
                        try self.executeThrow();
                    },
                    instructions.OpCode.OP_END_TRY => {
                        try self.executeEndTry();
                    },
                }
            } else {
                return error.NoActiveFrame;
            }
        }
    }

    fn i2f(self: *VM, a:  i32) !void {
        const result: f32 = @as(f32, @floatFromInt(a));
        self.stack.push(Frame.initFloat(result));
    }

    fn f2i(self: *VM, a: f32) !void {
        const result:  i32 = @intFromFloat(std.math.trunc(a));
        self.stack.push(Frame.initInt(result));
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

    fn logical_and(self: *VM, a: Frame, b: Frame) !Frame {
        if (a.value.type == .BOOL and b.value.type == .BOOL) {  
            const a_val = try a.asBoolean();
            const b_val = try b.asBoolean();
            return Frame.initBoolean(a_val and b_val);
        }
        self.reporter.reportFatalError("Cannot perform AND on non-boolean values", .{});
        return error.TypeError;
    }

    fn logical_or(self: *VM, a: Frame, b: Frame) !Frame {
        if (a.value.type == .BOOL and b.value.type == .BOOL) {
            const a_val = try a.asBoolean();
            const b_val = try b.asBoolean();
            return Frame.initBoolean(a_val or b_val);
        }
        self.reporter.reportFatalError("Cannot perform OR on non-boolean values", .{});
        return error.TypeError;
    }

    fn logical_not(self: *VM, a: Frame) !Frame {
        if (a.value.type == .BOOL) {
            const a_val = try a.asBoolean();
            return Frame.initBoolean(!a_val);
        }
        self.reporter.reportFatalError("Cannot perform NOT on a non-boolean value", .{});
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
        self.stack.push(Frame.initNothing(.INT));
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
            .sp = self.sp,
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

    fn concatStrings(self: *VM, a: *Frame, b: *Frame) !Frame {
        if (a.value.type == .STRING and b.value.type == .STRING) {
            const a_str = try a.asString();
            const b_str = try b.asString();
            
            // Allocate temporary buffer for concatStringsenation
            const new_string = try self.allocator.alloc(u8, a_str.len + b_str.len);
            defer self.allocator.free(new_string); // Free temporary buffer
            
            @memcpy(new_string[0..a_str.len], a_str);
            @memcpy(new_string[a_str.len..], b_str);
            
            // Create new frame with interned string
            return Frame.initString(&self.string_interner, new_string);
        }
        return error.TypeError;
    }

    fn concatArrays(self: *VM, a: *Frame, b: *Frame) !Frame {
        if (a.value.type == .ARRAY and b.value.type == .ARRAY) {
            const a_arr = a.value.data.array_val;
            const b_arr = b.value.data.array_val;
            
            // Create new array with combined capacity
            var result = try Frame.initArray(self.allocator, @intCast(a_arr.items.items.len + b_arr.items.items.len));
            errdefer result.deinit();
            
            // Copy items from both arrays
            try result.value.data.array_val.items.appendSlice(a_arr.items.items);
            try result.value.data.array_val.items.appendSlice(b_arr.items.items);
            
            // Intern the new array
            result.value.data.array_val = try self.array_interner.intern(result.value.data.array_val);
            result.owns_value = false;  // Array is now owned by the interner
            
            // Clean up original arrays
            a.deinit();
            b.deinit();
            
            return result;
        }
        return error.TypeError;
    }


    fn sliceArray(self: *VM, a: *Frame, start: u8, end: u8) !Frame {
        if (a.value.type == .ARRAY) {
            const array = a.value.data.array_val;
            const start_idx = @as(u32, start);
            const end_idx = @as(u32, end);
            
            // Bounds checking
            if (start_idx > end_idx or end_idx > array.items.items.len) {
                return error.IndexOutOfBounds;
            }
            
            // Create new array with the correct size
            var result = try Frame.initArray(self.allocator, @intCast(end_idx - start_idx));
            errdefer result.deinit();
            
            // Copy the slice of elements from the source array
            try result.value.data.array_val.items.appendSlice(
                array.items.items[start_idx..end_idx]
            );
            
            // Intern the new array
            result.value.data.array_val = try self.array_interner.intern(result.value.data.array_val);
            result.owns_value = false;  // Array is now owned by the interner
            
            // Don't deinit the original array since it's still on the stack
            return result;
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

    fn substr(self: *VM, a: *Frame, start: u8, len: u8) !Frame {
        if (a.value.type == .STRING) {
            const a_str = try a.asString();
            const start_idx = @as(u32, start);
            const length = @as(u32, len);
            
            if (start_idx >= a_str.len or start_idx + length > a_str.len) {
                return error.IndexOutOfBounds;
            }
            
            const slice = a_str[start_idx..][0..length];
            return Frame.initString(&self.string_interner, slice);
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

    fn executeMatch(self: *VM) !void {
        // Save match value from top of stack
        const match_value = self.pop() orelse return error.EmptyStack;
        if (!match_value.isEnum()) {
            return error.TypeMismatch;
        }
        self.match_value = match_value;
    }

    fn executeMatchArm(self: *VM) !void {
        const variant = self.read_byte();     // Get variant to compare
        const jump_offset = self.read_byte(); // Get jump offset
        
        const match_value = self.match_value.?;
        std.debug.print("Matching variant {} against {}\n", 
            .{variant, match_value.value.data.enum_val.variant});
        
        if (match_value.value.data.enum_val.variant != variant) {
            std.debug.print("No match, jumping {} bytes\n", .{jump_offset});
            // When jumping to else block, skip the if-block code
            self.current_frame.?.ip += jump_offset;
            self.ip = self.current_frame.?.ip;
            
            // Load var[0] for the else block
            if (self.current_scope_vars.items.len > 0) {
                const var_value = self.current_scope_vars.items[0];
                try self.stack.push(Frame.reference(var_value.value));
            }
        }
    }

    fn executeMatchEnd(self: *VM) !void {
        std.debug.print("Match end - Stack size: {}\n", .{self.stack.size()});
        self.match_value = null;  // Clear match value
    }

    fn executeTry(self: *VM) !void {
        
    }  

    fn executeCatch(self: *VM) !void {
        // TODO: Implement catch block
    }

    fn executeThrow(self: *VM) !void {
        self.reporter.reportFatalError("Thrown", .{});
        return error.Error;
    }

    fn executeEndTry(self: *VM) !void {
        // TODO: Implement end try
    }

};


pub fn main() !void {
    var gpa = std.heap.GeneralPurposeAllocator(.{}){};
    defer {
        const leaked = gpa.deinit();
        if (leaked == .leak) @panic("Memory leak detected!");
    }
    const allocator = gpa.allocator();

    var reporter = Reporting.initStderr();

    var interner = StringInterner.init(allocator);
    defer interner.deinit();

const code = [_]u8{
    // First set up our enum variable
    @intFromEnum(instructions.OpCode.OP_CONST), 0,         // Push enum value ONE
    @intFromEnum(instructions.OpCode.OP_SET_VAR), 0,       // Store in var[0]
    
    // Now do the match
    @intFromEnum(instructions.OpCode.OP_VAR), 0,           // Load x onto stack
    @intFromEnum(instructions.OpCode.OP_MATCH),            // Start match
    
    // Pattern ONE
    @intFromEnum(instructions.OpCode.OP_MATCH_ARM), 0, 5,  // Compare with variant 0, jump 5 bytes if no match
    @intFromEnum(instructions.OpCode.OP_CONST), 1,         // Push result 1
    @intFromEnum(instructions.OpCode.OP_JUMP), 10,         // Jump to end
    
    // Pattern TWO
    @intFromEnum(instructions.OpCode.OP_MATCH_ARM), 1, 5,  // Compare with variant 1, jump 5 bytes if no match
    @intFromEnum(instructions.OpCode.OP_CONST), 2,         // Push result 2
    @intFromEnum(instructions.OpCode.OP_JUMP), 5,          // Jump to end
    
    // Else branch
    @intFromEnum(instructions.OpCode.OP_CONST), 3,         // Push result 0
    
    @intFromEnum(instructions.OpCode.OP_MATCH_END),        // End match
    @intFromEnum(instructions.OpCode.OP_HALT),
};

var constants = [_]Frame{
    Frame{  // constant[0] = Enum.ONE
        .value = .{ 
            .type = .ENUM,
            .data = .{ .enum_val = .{ .type_name = "Number", .variant = 0 } },
            .nothing = false 
        },
        .allocator = null,
        .owns_value = true,
    },
    Frame{  // constant[1] = 1
        .value = .{ 
            .type = .INT, 
            .data = .{ .int = 1 }, 
            .nothing = false 
        },
        .allocator = null,
        .owns_value = true,
    },
    Frame{  // constant[2] = 2
        .value = .{ 
            .type = .INT, 
            .data = .{ .int = 2 }, 
            .nothing = false 
        },
        .allocator = null,
        .owns_value = true,
    },
};

    std.debug.print("\n=== Starting Conditional Block Test ===\n", .{});

    var vm = VM.init(allocator, &code, &constants, &reporter);
    defer vm.deinit();

    // Run the VM and handle results

    if (vm.eval()) |maybe_result| {
        if (maybe_result) |result| {
            std.debug.print("Final result: ", .{});
            VM.printValue(result.value);  // Call it as a static function
            std.debug.print("\n", .{});
            
            result.deinit();
            allocator.destroy(result);
        }
    } else |err| {
        std.debug.print("Error: {}\n", .{err});
    }
}