const std = @import("std");
const instructions = @import("instructions.zig");
const Reporting = @import("reporting.zig").Reporting;

// Using inline for compile-time evaluation
inline fn INT(x: u8) u8 {
    return instructions.Instruction.int(x);
}

const STACK_SIZE = 512;

pub const VM = struct {
    code: []const u8,
    constants: []const u8,
    //stack
    stack: [STACK_SIZE]instructions.Instruction,
    //stack pointer
    sp: usize,
    //instruction pointer
    ip: usize,
    reporter: *Reporting,

    pub fn init(code: []const u8, constants: []const u8, reporter: *Reporting) VM {
        var stack: [STACK_SIZE]instructions.Instruction = undefined;
        // Initialize all stack elements with OP_HALT
        for (&stack) |*slot| {
            slot.* = instructions.Instruction{ .OP_HALT = {} };
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

    fn push(self: *VM, value: instructions.Instruction) void {
        if (self.sp >= STACK_SIZE) {
            self.reporter.reportFatalError("Stack overflow", .{});
            return;
        }
        self.stack[self.sp] = value;
        self.sp += 1;
    }

    fn pop(self: *VM) ?instructions.Instruction {
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

    fn eval(self: *VM) ?instructions.Instruction {
        while (true) {
            const opcode: instructions.OpCode = @enumFromInt(self.read_byte());
            std.debug.print("Opcode: {}\n", .{opcode});
            switch (opcode) {
                // halt
                instructions.OpCode.OP_HALT => return if (self.sp > 0) self.stack[self.sp - 1] else null,
                // push constant
                instructions.OpCode.OP_CONST => {
                    const value = self.read_byte();
                    self.push(instructions.Instruction{ .OP_CONST = value });
                    std.debug.print("Pushed constant: {}\n", .{value});
                },
                // add
                instructions.OpCode.OP_ADD => {
                    const b = self.pop().?;
                    const a = self.pop().?;
                    switch (a) {
                        .OP_CONST => |a_val| {
                            switch (b) {
                                .OP_CONST => |b_val| {
                                    const result = instructions.Instruction{ .OP_CONST = a_val + b_val };
                                    self.push(result);
                                },
                                else => self.reporter.reportFatalError("Expected constant value", .{}),
                            }
                        },
                        else => self.reporter.reportFatalError("Expected constant value", .{}),
                    }
                },
                // sub
                instructions.OpCode.OP_SUB => {
                    const b = self.pop().?;
                    const a = self.pop().?;
                    switch (a) {
                        .OP_CONST => |a_val| {
                            switch (b) {
                                .OP_CONST => |b_val| {
                                    const result = instructions.Instruction{ .OP_CONST = a_val - b_val };
                                    self.push(result);
                                },
                                else => self.reporter.reportFatalError("Expected constant value", .{}),
                            }
                        },
                        else => self.reporter.reportFatalError("Expected constant value", .{}),
                    }
                },
                // mul
                instructions.OpCode.OP_MUL => {
                    const b = self.pop().?;
                    const a = self.pop().?;
                    switch (a) {
                        .OP_CONST => |a_val| {
                            switch (b) {
                                .OP_CONST => |b_val| {
                                    const result = instructions.Instruction{ .OP_CONST = a_val * b_val };
                                    self.push(result);
                                },
                                else => self.reporter.reportFatalError("Expected constant value", .{}),
                            }
                        },
                        else => self.reporter.reportFatalError("Expected constant value", .{}),
                    }
                },
                // div
                instructions.OpCode.OP_DIV => {
                    const b = self.pop().?;
                    const a = self.pop().?;
                    switch (a) {
                        .OP_CONST => |a_val| {
                            switch (b) {
                                .OP_CONST => |b_val| {
                                    const result = instructions.Instruction{ .OP_CONST = a_val / b_val };
                                    self.push(result);
                                },
                                else => self.reporter.reportFatalError("Expected constant value", .{}),
                            }
                        },
                        else => self.reporter.reportFatalError("Expected constant value", .{}),
                    }
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

};


pub fn main() void {
    const code = [_]u8{
        instructions.OpCode.OP_CONST.encode(),
        42,
        instructions.OpCode.OP_CONST.encode(),
        42,
        instructions.OpCode.OP_DIV.encode(),
        instructions.OpCode.OP_HALT.encode()
    };
    const constants = [_]u8{42};

    var reporter = Reporting.init();
    var vm = VM.init(&code, &constants, &reporter);
    
    if (vm.eval()) |result| {
        switch (result) {
            .OP_CONST => |value| std.debug.print("Thanks for using my VM! The answer is {}\n", .{value}),
            else => std.debug.print("Unexpected result type\n", .{}),
        }
    } else {
        std.debug.print("VM halted with empty stack\n", .{});
    }
}


           