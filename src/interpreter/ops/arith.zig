const std = @import("std");
const HIRValue = @import("../../codegen/hir/soxa_values.zig").HIRValue;
const Core = @import("../core.zig");
const HIRFrame = Core.HIRFrame;
const Errors = @import("../../utils/errors.zig");
const ErrorList = Errors.ErrorList;
const ErrorCode = Errors.ErrorCode;

pub fn exec(vm: anytype, a: anytype) !void {
    if (vm.stack.sp < 2) {
        return ErrorList.StackUnderflow;
    }
    const right = try vm.stack.pop();
    const left = try vm.stack.pop();

    if (a.operand_type == .Array) {
        if (a.op == .Add) {
            switch (left.value) {
                .array => |arr_a| {
                    switch (right.value) {
                        .array => |arr_b| {
                            var len_a: u32 = 0;
                            for (arr_a.elements) |elem| {
                                if (elem == .nothing) break;
                                len_a += 1;
                            }

                            var len_b: u32 = 0;
                            for (arr_b.elements) |elem| {
                                if (elem == .nothing) break;
                                len_b += 1;
                            }

                            const new_elements = try vm.scopeAllocator().alloc(HIRValue, len_a + len_b);

                            for (0..len_a) |i| {
                                new_elements[i] = arr_a.elements[i];
                            }

                            for (0..len_b) |i| {
                                new_elements[len_a + i] = arr_b.elements[i];
                            }

                            const result_array = HIRValue{
                                .array = .{
                                    .elements = new_elements,
                                    .capacity = len_a + len_b,
                                    .element_type = arr_a.element_type,
                                },
                            };

                            try vm.stack.push(HIRFrame.initFromHIRValue(result_array));
                            return;
                        },
                        else => {
                            return vm.reporter.reportRuntimeError(null, ErrorCode.VARIABLE_NOT_FOUND, "Cannot concatenate array with {s}", .{@tagName(right.value)});
                        },
                    }
                },
                else => {
                    return vm.reporter.reportRuntimeError(null, ErrorCode.VARIABLE_NOT_FOUND, "Cannot concatenate {s} with array", .{@tagName(left.value)});
                },
            }
        } else {
            return vm.reporter.reportRuntimeError(null, ErrorCode.VARIABLE_NOT_FOUND, "Cannot perform {s} operation on arrays", .{@tagName(a.op)});
        }
    }

    const left_type_rank: u8 = switch (left.value) {
        .float => 3,
        .int => 2,
        .byte => 1,
        else => 0,
    };

    const right_type_rank: u8 = switch (right.value) {
        .float => 3,
        .int => 2,
        .byte => 1,
        else => 0,
    };

    const target_rank = @max(left_type_rank, right_type_rank);

    if (target_rank == 3) {
        const left_float = switch (left.value) {
            .float => |f| f,
            .int => |i| @as(f64, @floatFromInt(i)),
            .byte => |u| @as(f64, @floatFromInt(u)),
            else => return vm.reporter.reportRuntimeError(null, ErrorCode.VARIABLE_NOT_FOUND, "Cannot convert {s} to float for arithmetic", .{@tagName(left.value)}),
        };

        const right_float = switch (right.value) {
            .float => |f| f,
            .int => |i| @as(f64, @floatFromInt(i)),
            .byte => |u| @as(f64, @floatFromInt(u)),
            else => return vm.reporter.reportRuntimeError(null, ErrorCode.VARIABLE_NOT_FOUND, "Cannot convert {s} to float for arithmetic", .{@tagName(right.value)}),
        };

        var result: f64 = 0.0;
        switch (a.op) {
            .Add => result = left_float + right_float,
            .Sub => result = left_float - right_float,
            .Mul => result = left_float * right_float,
            .Div => {
                if (right_float == 0.0) {
                    return ErrorList.DivisionByZero;
                } else result = left_float / right_float;
            },
            .Mod => result = @mod(left_float, right_float),
            .Pow => {
                result = std.math.pow(f64, left_float, right_float);
            },
        }

        try vm.stack.push(HIRFrame.initFloat(result));
        return;
    }

    if (false) {
        const left_float = switch (left.value) {
            .int => |i| @as(f64, @floatFromInt(i)),
            .byte => |u| @as(f64, @floatFromInt(u)),
            .float => |f| f,
            .string => |s| blk: {
                const parsed = std.fmt.parseFloat(f64, s) catch {
                    return vm.reporter.reportRuntimeError(null, ErrorCode.VARIABLE_NOT_FOUND, "Cannot convert string '{s}' to float for arithmetic", .{s});
                };
                break :blk parsed;
            },
            else => {
                return vm.reporter.reportRuntimeError(null, ErrorCode.VARIABLE_NOT_FOUND, "Cannot convert {s} to float for arithmetic", .{@tagName(left.value)});
            },
        };

        const right_float = switch (right.value) {
            .int => |i| @as(f64, @floatFromInt(i)),
            .byte => |u| @as(f64, @floatFromInt(u)),
            .float => |f| f,
            .string => |s| blk: {
                const parsed = std.fmt.parseFloat(f64, s) catch {
                    return vm.reporter.reportRuntimeError(null, ErrorCode.VARIABLE_NOT_FOUND, "Cannot convert string '{s}' to float for arithmetic", .{s});
                };
                break :blk parsed;
            },
            else => {
                return vm.reporter.reportRuntimeError(null, ErrorCode.VARIABLE_NOT_FOUND, "Cannot convert {s} to float for arithmetic", .{@tagName(right.value)});
            },
        };

        var result: f64 = 0.0;
        switch (a.op) {
            .Add => result = left_float + right_float,
            .Sub => result = left_float - right_float,
            .Mul => result = left_float * right_float,
            .Div => {
                if (right_float == 0.0) {
                    return ErrorList.DivisionByZero;
                } else result = left_float / right_float;
            },
            .Mod => result = @mod(left_float, right_float),
            .Pow => {
                result = std.math.pow(f64, left_float, right_float);
            },
        }

        try vm.stack.push(HIRFrame.initFloat(result));
        return;
    }

    if (target_rank == 1) {
        const left_byte = switch (left.value) {
            .byte => |u| u,
            else => return vm.reporter.reportRuntimeError(null, ErrorCode.VARIABLE_NOT_FOUND, "Cannot convert {s} to byte for arithmetic", .{@tagName(left.value)}),
        };

        const right_byte = switch (right.value) {
            .byte => |u| u,
            else => return vm.reporter.reportRuntimeError(null, ErrorCode.VARIABLE_NOT_FOUND, "Cannot convert {s} to byte for arithmetic", .{@tagName(right.value)}),
        };

        var byte_result: u8 = undefined;
        switch (a.op) {
            .Add => {
                const sum: u16 = @as(u16, left_byte) + @as(u16, right_byte);
                byte_result = if (sum > 255) 255 else @intCast(sum);
            },
            .Sub => {
                byte_result = if (left_byte >= right_byte) left_byte - right_byte else 0;
            },
            .Mul => {
                const prod: u32 = @as(u32, left_byte) * @as(u32, right_byte);
                byte_result = if (prod > 255) 255 else @intCast(prod);
            },
            .Div => {
                if (right_byte == 0) {
                    return vm.reporter.reportRuntimeError(null, ErrorCode.DIVISION_BY_ZERO, "Division by zero in byte arithmetic", .{});
                }
                byte_result = @intCast(left_byte / right_byte);
            },
            .Mod => byte_result = @as(u8, @intCast(@mod(left_byte, right_byte))),
            .Pow => {
                const p: u32 = std.math.pow(u32, @as(u32, left_byte), @as(u32, right_byte));
                byte_result = if (p > 255) 255 else @intCast(p);
            },
        }

        try vm.stack.push(HIRFrame.initByte(byte_result));
        return;
    }

    const left_int = switch (left.value) {
        .int => |i| i,
        .byte => |u| @as(i64, u),
        .tetra => |t| @as(i64, t),
        .string => |s| blk: {
            const parsed = std.fmt.parseInt(i64, s, 10) catch {
                return vm.reporter.reportRuntimeError(null, ErrorCode.VARIABLE_NOT_FOUND, "Cannot convert string to integer for arithmetic", .{});
            };
            break :blk parsed;
        },
        .nothing => {
            return vm.reporter.reportRuntimeError(null, ErrorCode.VARIABLE_NOT_FOUND, "Cannot perform arithmetic on 'nothing' value", .{});
        },
        else => {
            return vm.reporter.reportRuntimeError(null, ErrorCode.VARIABLE_NOT_FOUND, "Cannot convert {s} to integer for arithmetic", .{@tagName(left.value)});
        },
    };

    const right_int = switch (right.value) {
        .int => |i| i,
        .byte => |u| @as(i64, u),
        .tetra => |t| @as(i64, t),
        .string => |s| blk: {
            const parsed = std.fmt.parseInt(i64, s, 10) catch {
                return vm.reporter.reportRuntimeError(null, ErrorCode.VARIABLE_NOT_FOUND, "Cannot convert string to integer for arithmetic", .{});
            };
            break :blk parsed;
        },
        .nothing => {
            return vm.reporter.reportRuntimeError(null, ErrorCode.VARIABLE_NOT_FOUND, "Cannot perform arithmetic on 'nothing' value", .{});
        },
        else => {
            return vm.reporter.reportRuntimeError(null, ErrorCode.VARIABLE_NOT_FOUND, "Cannot convert {s} to integer for arithmetic", .{@tagName(right.value)});
        },
    };

    var int_result: i64 = undefined;
    switch (a.op) {
        .Add => int_result = std.math.add(i64, left_int, right_int) catch {
            return vm.reporter.reportRuntimeError(null, ErrorCode.ARITHMETIC_OVERFLOW, "Integer addition overflow", .{});
        },
        .Sub => int_result = std.math.sub(i64, left_int, right_int) catch {
            return vm.reporter.reportRuntimeError(null, ErrorCode.ARITHMETIC_OVERFLOW, "Integer subtraction overflow", .{});
        },
        .Mul => int_result = std.math.mul(i64, left_int, right_int) catch {
            return vm.reporter.reportRuntimeError(null, ErrorCode.ARITHMETIC_OVERFLOW, "Integer multiplication overflow", .{});
        },
        .Div => {
            const left_float = @as(f64, @floatFromInt(left_int));
            const right_float = @as(f64, @floatFromInt(right_int));
            if (right_float == 0.0) {
                return ErrorList.DivisionByZero;
            }
            const result = left_float / right_float;
            try vm.stack.push(HIRFrame.initFloat(result));
            return;
        },
        .Mod => int_result = try fastIntMod(vm, left_int, right_int),
        .Pow => {
            int_result = std.math.pow(i64, left_int, right_int);
        },
    }

    if (left.value == .byte and right.value == .byte and int_result >= 0 and int_result <= 255) {
        try vm.stack.push(HIRFrame.initByte(@intCast(int_result)));
    } else {
        try vm.stack.push(HIRFrame.initInt(int_result));
    }
}

pub fn intDiv(_: anytype, a: i64, b: i64) !i64 {
    if (b == 0) {
        return ErrorList.DivisionByZero;
    }
    return @divTrunc(a, b);
}

pub fn fastIntMod(_: anytype, a: i64, b: i64) !i64 {
    if (b == 0) {
        return ErrorList.DivisionByZero;
    }
    return switch (b) {
        3 => fastMod3(a),
        5 => fastMod5(a),
        15 => fastMod15(a),
        else => @mod(a, b),
    };
}

inline fn fastMod3(n: i64) i64 {
    var x = n;
    if (x < 0) x = -x;
    while (x >= 3) {
        x = (x >> 2) + (x & 3);
        if (x >= 3) x -= 3;
    }
    return if (n < 0 and x != 0) 3 - x else x;
}

inline fn fastMod5(n: i64) i64 {
    return @mod(n, 5);
}

inline fn fastMod15(n: i64) i64 {
    const mod3 = fastMod3(n);
    const mod5 = fastMod5(n);
    return @mod(mod3 + 3 * @mod(mod5 * 2, 5), 15);
}
