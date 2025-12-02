const std = @import("std");
const HIRValue = @import("../../codegen/hir/soxa_values.zig").HIRValue;
const HIRArray = @import("../../codegen/hir/soxa_values.zig").HIRArray;
const Core = @import("../core.zig");
const HIRFrame = Core.HIRFrame;
const Errors = @import("../../utils/errors.zig");
const ErrorList = Errors.ErrorList;
const ErrorCode = Errors.ErrorCode;
const meta = std.meta;

fn runtimePanic(vm: anytype, err: ErrorList, code: []const u8, comptime fmt: []const u8, args: anytype) ErrorList {
    vm.reporter.reportRuntimeError(null, code, fmt, args);
    vm.running = false;
    return err;
}

fn convertToInt(vm: anytype, value: HIRValue) ErrorList!i64 {
    return switch (value) {
        .int => value.int,
        .byte => |b| @as(i64, @intCast(b)),
        .float => |f| blk: {
            if (!std.math.isFinite(f)) {
                return runtimePanic(vm, ErrorList.FloatOverflow, ErrorCode.INVALID_ARGUMENT, "@int: non-finite float cannot convert to int", .{});
            }
            const max = @as(f64, @floatFromInt(std.math.maxInt(i64)));
            const min = @as(f64, @floatFromInt(std.math.minInt(i64)));
            if (f > max or f < min) {
                return runtimePanic(vm, ErrorList.IntegerOverflow, ErrorCode.ARITHMETIC_OVERFLOW, "@int: float {d} outside int range", .{f});
            }
            break :blk @as(i64, @intFromFloat(f));
        },
        .string => |s_val| blk: {
            // Trim leading and trailing whitespace
            const trimmed = std.mem.trim(u8, s_val, &std.ascii.whitespace);

            if (trimmed.len > 2 and std.mem.eql(u8, trimmed[0..2], "0x")) {
                const hex_str = trimmed[2..];
                const parsed_hex = std.fmt.parseInt(i64, hex_str, 16) catch {
                    return runtimePanic(vm, ErrorList.InvalidNumber, ErrorCode.INVALID_ARGUMENT, "@int: invalid hex literal '{s}'", .{s_val});
                };
                break :blk parsed_hex;
            }

            const parsed_int_opt: ?i64 = std.fmt.parseInt(i64, trimmed, 10) catch null;
            if (parsed_int_opt) |parsed_int| {
                break :blk parsed_int;
            }

            const parsed_float = std.fmt.parseFloat(f64, trimmed) catch {
                return runtimePanic(vm, ErrorList.InvalidNumber, ErrorCode.INVALID_ARGUMENT, "@int: failed to parse '{s}'", .{s_val});
            };
            if (!std.math.isFinite(parsed_float)) {
                return runtimePanic(vm, ErrorList.FloatOverflow, ErrorCode.INVALID_ARGUMENT, "@int: '{s}' is not a finite number", .{s_val});
            }
            const max = @as(f64, @floatFromInt(std.math.maxInt(i64)));
            const min = @as(f64, @floatFromInt(std.math.minInt(i64)));
            if (parsed_float > max or parsed_float < min) {
                return runtimePanic(vm, ErrorList.IntegerOverflow, ErrorCode.ARITHMETIC_OVERFLOW, "@int: value '{s}' outside int range", .{s_val});
            }
            break :blk @as(i64, @intFromFloat(parsed_float));
        },
        else => return runtimePanic(vm, ErrorList.TypeError, ErrorCode.TYPE_MISMATCH, "@int: cannot convert value of type {s}", .{@tagName(meta.activeTag(value))}),
    };
}

fn convertToFloat(vm: anytype, value: HIRValue) ErrorList!f64 {
    return switch (value) {
        .float => value.float,
        .int => |i| @as(f64, @floatFromInt(i)),
        .byte => |b| @as(f64, @floatFromInt(b)),
        .string => |s_val| blk: {
            if (s_val.len > 2 and std.mem.eql(u8, s_val[0..2], "0x")) {
                const hex_str = s_val[2..];
                const parsed_hex = std.fmt.parseInt(u64, hex_str, 16) catch {
                    return runtimePanic(vm, ErrorList.InvalidNumber, ErrorCode.INVALID_ARGUMENT, "@float: invalid hex literal '{s}'", .{s_val});
                };
                break :blk @as(f64, @floatFromInt(parsed_hex));
            }

            const parsed = std.fmt.parseFloat(f64, s_val) catch {
                return runtimePanic(vm, ErrorList.InvalidNumber, ErrorCode.INVALID_ARGUMENT, "@float: failed to parse '{s}'", .{s_val});
            };
            if (!std.math.isFinite(parsed)) {
                return runtimePanic(vm, ErrorList.FloatOverflow, ErrorCode.INVALID_ARGUMENT, "@float: '{s}' is not a finite number", .{s_val});
            }
            break :blk parsed;
        },
        else => return runtimePanic(vm, ErrorList.TypeError, ErrorCode.TYPE_MISMATCH, "@float: cannot convert value of type {s}", .{@tagName(meta.activeTag(value))}),
    };
}

fn convertToByte(vm: anytype, value: HIRValue) ErrorList!u8 {
    return switch (value) {
        .byte => value.byte,
        .int => |i| blk: {
            if (i >= 0 and i <= 255) {
                break :blk @as(u8, @intCast(i));
            }
            return runtimePanic(vm, ErrorList.byteOverflow, ErrorCode.INVALID_ARGUMENT, "@byte: integer {} out of byte range", .{i});
        },
        .float => |f| blk: {
            if (!std.math.isFinite(f)) {
                return runtimePanic(vm, ErrorList.FloatOverflow, ErrorCode.INVALID_ARGUMENT, "@byte: non-finite float cannot convert to byte", .{});
            }
            const rounded = @as(i64, @intFromFloat(f));
            if (rounded >= 0 and rounded <= 255) {
                break :blk @as(u8, @intCast(rounded));
            }
            return runtimePanic(vm, ErrorList.byteOverflow, ErrorCode.INVALID_ARGUMENT, "@byte: float {d} out of byte range", .{f});
        },
        .string => |s_val| blk: {
            if (s_val.len == 0) {
                return runtimePanic(vm, ErrorList.InvalidNumber, ErrorCode.INVALID_ARGUMENT, "@byte: empty string", .{});
            }

            if (s_val.len == 1) {
                break :blk s_val[0];
            }

            if (s_val.len > 2 and std.mem.eql(u8, s_val[0..2], "0x")) {
                const hex_str = s_val[2..];
                const parsed_hex_byte = std.fmt.parseInt(u8, hex_str, 16) catch {
                    return runtimePanic(vm, ErrorList.InvalidNumber, ErrorCode.INVALID_ARGUMENT, "@byte: invalid hex literal '{s}'", .{s_val});
                };
                break :blk parsed_hex_byte;
            }

            const parsed_int_opt: ?i64 = std.fmt.parseInt(i64, s_val, 10) catch null;
            if (parsed_int_opt) |parsed_int| {
                if (parsed_int >= 0 and parsed_int <= 255) {
                    break :blk @as(u8, @intCast(parsed_int));
                }
                return runtimePanic(vm, ErrorList.byteOverflow, ErrorCode.INVALID_ARGUMENT, "@byte: integer {d} out of byte range", .{parsed_int});
            }

            const parsed_float = std.fmt.parseFloat(f64, s_val) catch {
                return runtimePanic(vm, ErrorList.InvalidNumber, ErrorCode.INVALID_ARGUMENT, "@byte: failed to parse '{s}'", .{s_val});
            };
            if (!std.math.isFinite(parsed_float)) {
                return runtimePanic(vm, ErrorList.FloatOverflow, ErrorCode.INVALID_ARGUMENT, "@byte: '{s}' is not a finite number", .{s_val});
            }
            const rounded = @as(i64, @intFromFloat(parsed_float));
            if (rounded >= 0 and rounded <= 255) {
                break :blk @as(u8, @intCast(rounded));
            }
            return runtimePanic(vm, ErrorList.byteOverflow, ErrorCode.INVALID_ARGUMENT, "@byte: float {d} out of byte range", .{parsed_float});
        },
        else => return runtimePanic(vm, ErrorList.TypeError, ErrorCode.TYPE_MISMATCH, "@byte: cannot convert value of type {s}", .{@tagName(meta.activeTag(value))}),
    };
}

pub fn exec(vm: anytype, s: anytype) !void {
    const val = try vm.stack.pop();

    if (s.op == .Length) {
        switch (val.value) {
            .string => |sv| {
                const len = @as(i64, @intCast(sv.len));
                try vm.stack.push(HIRFrame.initInt(len));
            },
            .array => |arr| {
                var logical_len: usize = 0;
                while (logical_len < arr.elements.len) : (logical_len += 1) {
                    if (arr.elements[logical_len] == .nothing) break;
                }
                try vm.stack.push(HIRFrame.initInt(@intCast(logical_len)));
            },
            else => return ErrorList.TypeError,
        }
        return;
    }

    if (s.op == .ToString) {
        const value_ptr = try vm.allocator.create(HIRValue);
        value_ptr.* = val.value;
        defer vm.allocator.destroy(value_ptr);
        const result = try vm.valueToString(value_ptr);
        try vm.stack.push(HIRFrame.initString(result));
        return;
    }

    if (s.op == .ToInt) {
        const result = try convertToInt(vm, val.value);
        try vm.stack.push(HIRFrame.initInt(result));
        return;
    }
    if (s.op == .ToFloat) {
        const result = try convertToFloat(vm, val.value);
        try vm.stack.push(HIRFrame.initFloat(result));
        return;
    }
    if (s.op == .ToByte) {
        const result = try convertToByte(vm, val.value);
        try vm.stack.push(HIRFrame.initByte(result));
        return;
    }

    switch (val.value) {
        .string => |s_val| {
            switch (s.op) {
                .Pop => {
                    if (s_val.len == 0) {
                        return vm.reporter.reportRuntimeError(null, ErrorCode.VARIABLE_NOT_FOUND, "Cannot pop from empty string", .{});
                    }

                    var last_char_start: usize = s_val.len;
                    var i: usize = s_val.len;
                    while (i > 0) {
                        i -= 1;
                        const byte = s_val[i];
                        if ((byte & 0x80) == 0) {
                            last_char_start = i;
                            break;
                        } else if ((byte & 0xC0) == 0xC0) {
                            last_char_start = i;
                            break;
                        }
                    }

                    const last_char = s_val[last_char_start..s_val.len];
                    const remaining = s_val[0..last_char_start];

                    try vm.stack.push(HIRFrame.initString(remaining));
                    try vm.stack.push(HIRFrame.initString(last_char));
                },
                .Substring => {
                    const length = try vm.stack.pop();
                    const start = try vm.stack.pop();

                    if (start.value != .int or length.value != .int) {
                        return ErrorList.TypeError;
                    }

                    const start_i: i64 = start.value.int;
                    const len_i: i64 = length.value.int;
                    if (start_i < 0 or len_i < 0) {
                        return runtimePanic(vm, ErrorList.IndexOutOfBounds, ErrorCode.INDEX_OUT_OF_BOUNDS, "@slice: start and length must be non-negative", .{});
                    }

                    const start_idx = @as(usize, @intCast(start_i));
                    const len = @as(usize, @intCast(len_i));

                    if (start_idx >= s_val.len or start_idx + len > s_val.len) {
                        return runtimePanic(vm, ErrorList.IndexOutOfBounds, ErrorCode.INDEX_OUT_OF_BOUNDS, "@slice: range {d}:{d} outside string of length {d}", .{ start_i, len_i, s_val.len });
                    }
                    const end_idx = start_idx + len;

                    const substring = s_val[start_idx..end_idx];
                    try vm.stack.push(HIRFrame.initString(substring));
                },
                .Concat => {
                    const str2 = try vm.stack.pop();
                    if (str2.value != .string) {
                        return ErrorList.TypeError;
                    }

                    const result = try std.fmt.allocPrint(vm.allocator, "{s}{s}", .{ s_val, str2.value.string });
                    try vm.stack.push(HIRFrame.initString(result));
                },
                .ToInt => {
                    const converted = try convertToInt(vm, HIRValue{ .string = s_val });
                    try vm.stack.push(HIRFrame.initInt(converted));
                },
                .ToString => {
                    try vm.stack.push(HIRFrame.initString(s_val));
                },
                else => return ErrorList.TypeError,
            }
        },
        else => return ErrorList.TypeError,
    }
}

pub fn stringConcat(vm: anytype, a: HIRFrame, b: HIRFrame) !HIRFrame {
    const a_str = switch (a.value) {
        .string => |s| s,
        else => return ErrorList.TypeError,
    };

    const b_str = switch (b.value) {
        .string => |s| s,
        else => return ErrorList.TypeError,
    };

    const new_string = try vm.allocator.alloc(u8, a_str.len + b_str.len);
    // TODO: Integrate with string interning

    @memcpy(new_string[0..a_str.len], a_str);
    @memcpy(new_string[a_str.len..], b_str);

    return HIRFrame.initString(new_string);
}

pub fn stringLength(vm: anytype, a: HIRFrame) !HIRFrame {
    _ = vm;
    const str = switch (a.value) {
        .string => |s| s,
        else => return ErrorList.TypeError,
    };

    return HIRFrame.initInt(@as(i64, @intCast(str.len)));
}

pub fn stringBytes(vm: anytype, a: HIRFrame) !HIRFrame {
    const str = switch (a.value) {
        .string => |s| s,
        else => return ErrorList.TypeError,
    };

    const elements = try vm.allocator.alloc(HIRValue, str.len);
    for (str, 0..) |byte, i| {
        elements[i] = HIRValue{ .byte = byte };
    }
    const array = HIRValue{ .array = HIRArray{
        .elements = elements,
        .element_type = .Byte,
        .capacity = @intCast(str.len),
    } };
    return HIRFrame.initFromHIRValue(array);
}

pub fn stringSubstring(vm: anytype, str_frame: HIRFrame, start_frame: HIRFrame, len_frame: HIRFrame) !HIRFrame {
    const str = switch (str_frame.value) {
        .string => |s| s,
        else => return ErrorList.TypeError,
    };

    const start = switch (start_frame.value) {
        .int => |i| i,
        else => return ErrorList.TypeError,
    };

    const length = switch (len_frame.value) {
        .int => |i| i,
        else => return ErrorList.TypeError,
    };

    if (start < 0 or length < 0 or start >= str.len or start + length > str.len) {
        return ErrorList.IndexOutOfBounds;
    }

    const start_idx = @as(usize, @intCast(start));
    const len_val = @as(usize, @intCast(length));
    const slice = str[start_idx .. start_idx + len_val];

    // TODO: Use string interning
    const new_string = try vm.allocator.dupe(u8, slice);
    return HIRFrame.initString(new_string);
}
