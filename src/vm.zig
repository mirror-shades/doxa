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
    
    pub fn asInt(self: Frame) !i64 {
        return self.value.data.int;
    }

    pub fn asFloat(self: Frame) !f64 {
        return self.value.data.float;
    }

    pub fn asBoolean(self: Frame) !bool {
        return self.value.data.boolean;
    }

    pub fn asString(self: Frame) ![]const u8 {
        return self.value.data.string;
    }
};

const STACK_SIZE = 512;

pub const VM = struct {
    code: []const u8,
    constants: []const u8,
    //stack
    stack: [STACK_SIZE]Frame,
    //stack pointer
    sp: usize,
    //instruction pointer
    ip: usize,
    reporter: *Reporting,

    pub fn init(code: []const u8, constants: []const u8, reporter: *Reporting) VM {
        var stack: [STACK_SIZE]Frame = undefined;
        // Initialize all stack elements with NOTHING
        for (&stack) |*slot| {
            slot.* = Frame.initNothing();
        }

        return VM{
            .ip = 0,
            .sp = 0,
            .stack = stack,
            .constants = constants,
            .code = code,
            .reporter = reporter,
        };
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

    fn get_constant(self: *VM, index: u8) u8 {
        return self.constants[index];
    }

    fn eval(self: *VM) ?Frame {
        while (true) {
            const opcode: instructions.OpCode = @enumFromInt(self.read_byte());
            std.debug.print("Opcode: {}\n", .{opcode});
            switch (opcode) {
                // halt
                instructions.OpCode.OP_HALT => return if (self.sp > 0) self.stack[self.sp - 1] else null,
                // push constant
                instructions.OpCode.OP_CONST => {
                    const value = self.read_byte();
                    self.push(Frame.initInt(value));
                    std.debug.print("Pushed constant: {}\n", .{value});
                },
                // push variable
                instructions.OpCode.OP_VAR => {
                    const value = self.read_byte();
                    self.push(Frame.initInt(value));
                    std.debug.print("Pushed variable: {}\n", .{value});
                },
                // add
                instructions.OpCode.OP_IADD => {
                    const b = self.pop().?;
                    const a = self.pop().?;
                    const result = i_add(a.asInt() catch return null, b.asInt() catch return null) catch return null;
                    self.push(result);
                },
                // sub
                instructions.OpCode.OP_ISUB => {
                    const b = self.pop().?;
                    const a = self.pop().?;
                    const result = i_sub(a.asInt() catch return null, b.asInt() catch return null) catch return null;
                    self.push(result);
                },
                // mul
                instructions.OpCode.OP_IMUL => {
                    const b = self.pop().?;
                    const a = self.pop().?;
                    const result = i_mul(a.asInt() catch return null, b.asInt() catch return null) catch return null;
                    self.push(result);
                },
                // div
                instructions.OpCode.OP_IDIV => {
                    const b = self.pop().?;
                    const a = self.pop().?;
                    const result = i_div(a.asInt() catch return null, b.asInt() catch return null) catch return null;
                    self.push(result);
                },
                // fadd
                instructions.OpCode.OP_FADD => {
                    const b = self.pop().?;
                    const a = self.pop().?;
                    const result = f_add(a.asFloat() catch return null, b.asFloat() catch return null) catch return null;
                    self.push(result);
                },

                // fsub
                instructions.OpCode.OP_FSUB => {
                    const b = self.pop().?;
                    const a = self.pop().?;
                    const result = f_sub(a.asFloat() catch return null, b.asFloat() catch return null) catch return null;
                    self.push(result);
                },
                // fmul
                instructions.OpCode.OP_FMUL => {
                    const b = self.pop().?;
                    const a = self.pop().?;
                    const result = f_mul(a.asFloat() catch return null, b.asFloat() catch return null) catch return null;
                    self.push(result);
                },
                // fdiv
                instructions.OpCode.OP_FDIV => {
                    const b = self.pop().?;
                    const a = self.pop().?;
                    const result = f_div(a.asFloat() catch return null, b.asFloat() catch return null) catch return null;
                    self.push(result);
                },
                // i2f
                instructions.OpCode.OP_I2F => {
                    const value = self.pop().?;
                    try self.i2f(value.asInt() catch return null);
                },
                // f2i
                instructions.OpCode.OP_F2I => {
                    const value = self.pop().?;
                    try self.f2i(value.asFloat() catch return null);
                },
            }
        }
        
        // Print final stack state
        std.debug.print("Final stack (sp={}): ", .{self.sp});
        var i: usize = 0;
        while (i < self.sp) : (i += 1) {
            switch (self.stack[i]) {
                .OP_CONST => |value| std.debug.print("{} ", .{value}),
                .OP_HALT => std.debug.print("HALT ", .{}),
            }
        }
        std.debug.print("\n", .{});
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
        if (a + b < std.math.maxInt(i64) or a + b > std.math.minInt(i64)) {
            return error.IntegerOverflow;
        }
        return Frame.initInt(a + b);
    }

    fn i_sub(a: i64, b: i64) !Frame {
        if (a - b < std.math.maxInt(i64) or a - b > std.math.minInt(i64)) {
            return error.IntegerOverflow;
        }
        return Frame.initInt(a - b);
    }

    fn i_mul(a: i64, b: i64) !Frame {
        if (a * b < std.math.maxInt(i64) or a * b > std.math.minInt(i64)) {
            return error.IntegerOverflow;
        }
        return Frame.initInt(a * b);
    }

    fn i_div(a: i64, b: i64) !Frame {
        if (b == 0) {
            return error.DivisionByZero;
        }
        const af = @as(f64, @floatFromInt(a));
        const bf = @as(f64, @floatFromInt(b));
        return Frame.initFloat(std.math.trunc(af / bf));
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

    fn halt(self: *VM) !void {
        self.push(Frame.initNothing());
    }
    };



pub fn main() void {
    const code = [_]u8{
        instructions.OpCode.OP_CONST.encode(),
        42,
        instructions.OpCode.OP_CONST.encode(),
        42,
        instructions.OpCode.OP_IDIV.encode(),
        instructions.OpCode.OP_HALT.encode()
    };
    const constants = [_]u8{42};

    var reporter = Reporting.init();
    var vm = VM.init(&code, &constants, &reporter);
    
    if (vm.eval()) |frame| {
        switch (frame.value.type) {
            .INT => std.debug.print("Thanks for using my VM! The answer is {}\n", .{frame.value.data.int}),
            .FLOAT => std.debug.print("Thanks for using my VM! The answer is {d}\n", .{frame.value.data.float}),
            .STRING => std.debug.print("Thanks for using my VM! The answer is {s}\n", .{frame.value.data.string}),
            .BOOL => std.debug.print("Thanks for using my VM! The answer is {}\n", .{frame.value.data.boolean}),
            .NOTHING => std.debug.print("VM halted with nothing\n", .{}),
            else => std.debug.print("Unknown result type\n", .{}),
        }
    } else {
        std.debug.print("VM halted with empty stack\n", .{});
    }
}


           