const std = @import("std");
const HIRValue = @import("../../codegen/hir/soxa_values.zig").HIRValue;
const HIRArray = @import("../../codegen/hir/soxa_values.zig").HIRArray;
const Core = @import("../core.zig");
const HIRFrame = Core.HIRFrame;
const Errors = @import("../../utils/errors.zig");
const ErrorList = Errors.ErrorList;
const ErrorCode = Errors.ErrorCode;

// Execute the StringOp instruction. Accepts the VM as `anytype` to avoid import cycles.
pub fn exec(vm: anytype, s: anytype) !void {
    const val = try vm.stack.pop();

    if (vm.reporter.debug_mode) {
        const t = vm.getTypeString(val.value);
        vm.reporter.report(.Debug, .Hint, null, null, "StringOp.{s} in_type='{s}'", .{ @tagName(s.op), t });
    }

    // Fast-path for Length which is valid for both strings and arrays
    if (s.op == .Length) {
        switch (val.value) {
            .string => |sv| {
                const len = @as(i64, @intCast(sv.len));
                try vm.stack.push(HIRFrame.initInt(len));
            },
            .array => |arr| {
                // Use logical length: count elements until first 'nothing'
                var logical_len: usize = 0;
                while (logical_len < arr.elements.len) : (logical_len += 1) {
                    if (std.meta.eql(arr.elements[logical_len], HIRValue.nothing)) break;
                }
                try vm.stack.push(HIRFrame.initInt(@intCast(logical_len)));
            },
            else => return ErrorList.TypeError,
        }
        return; // done handling Length
    }

    // Fast-path for ToString which is valid for any type
    if (s.op == .ToString) {
        const result = try vm.valueToString(val.value);
        try vm.stack.push(HIRFrame.initString(result));
        return; // done handling ToString
    }

    // Numeric conversions: accept string, int, float, byte
    if (s.op == .ToInt) {
        const out: HIRValue = switch (val.value) {
            .int => val.value,
            .byte => |u| HIRValue{ .int = @as(i64, u) },
            .float => |f| HIRValue{ .int = @as(i64, @intFromFloat(f)) },
            .string => |s_val| blk: {
                const parsed = std.fmt.parseInt(i64, s_val, 10) catch {
                    // On failure, mirror existing behavior: return NumberError.ParseFailed if available; else 0
                    var variant_index: u32 = 0;
                    if (vm.custom_type_registry.get("NumberError")) |ct| {
                        if (ct.enum_variants) |variants| {
                            for (variants, 0..) |v, i| {
                                if (std.mem.eql(u8, v.name, "ParseFailed")) {
                                    variant_index = @intCast(i);
                                    break;
                                }
                            }
                        }
                    }
                    break :blk HIRValue{ .enum_variant = .{ .type_name = "NumberError", .variant_name = "ParseFailed", .variant_index = variant_index } };
                };
                if (vm.reporter.debug_mode) {
                    vm.reporter.report(.Debug, .Hint, null, null, "StringOp.ToInt parsed='{d}'", .{parsed});
                }
                break :blk HIRValue{ .int = parsed };
            },
            else => HIRValue{ .int = 0 },
        };
        try vm.stack.push(HIRFrame.initFromHIRValue(out));
        return;
    }
    if (s.op == .ToFloat) {
        const out: HIRValue = switch (val.value) {
            .float => val.value,
            .int => |i| HIRValue{ .float = @as(f64, @floatFromInt(i)) },
            .byte => |u| HIRValue{ .float = @as(f64, @floatFromInt(u)) },
            .string => |s_val| blk: {
                const parsed = std.fmt.parseFloat(f64, s_val) catch 0.0;
                break :blk HIRValue{ .float = parsed };
            },
            else => HIRValue{ .float = 0.0 },
        };
        try vm.stack.push(HIRFrame.initFromHIRValue(out));
        return;
    }
    if (s.op == .ToByte) {
        const out: HIRValue = switch (val.value) {
            .byte => val.value,
            .int => |i| HIRValue{ .byte = if (i >= 0 and i <= 255) @intCast(i) else 0 },
            .float => |f| blk: {
                const i: i64 = @as(i64, @intFromFloat(f));
                break :blk HIRValue{ .byte = if (i >= 0 and i <= 255) @intCast(i) else 0 };
            },
            .string => |s_val| blk: {
                const parsed = std.fmt.parseInt(i64, s_val, 10) catch 0;
                break :blk HIRValue{ .byte = if (parsed >= 0 and parsed <= 255) @intCast(parsed) else 0 };
            },
            else => HIRValue{ .byte = 0 },
        };
        try vm.stack.push(HIRFrame.initFromHIRValue(out));
        return;
    }

    // For the remaining ops we expect a string value
    switch (val.value) {
        .string => |s_val| {
            switch (s.op) {
                .Pop => {
                    // Pop last character: return the last character as string and update original string
                    if (s_val.len == 0) {
                        return vm.reporter.reportRuntimeError(null, ErrorCode.VARIABLE_NOT_FOUND, "Cannot pop from empty string", .{});
                    }
                    const last_index = s_val.len - 1;
                    const last_char = s_val[last_index..s_val.len];
                    const remaining = s_val[0..last_index];
                    // Push updated string first, then popped char to mirror ArrayPop order
                    try vm.stack.push(HIRFrame.initString(remaining));
                    try vm.stack.push(HIRFrame.initString(last_char));
                },
                .Substring => {
                    // Get substring parameters
                    const length = try vm.stack.pop();
                    const start = try vm.stack.pop();

                    // Validate parameters
                    if (start.value != .int or length.value != .int) {
                        return ErrorList.TypeError;
                    }

                    // Normalize and validate parameters (including negatives)
                    const start_i: i64 = start.value.int;
                    const len_i: i64 = length.value.int;
                    if (start_i < 0 or len_i < 0) {
                        var variant_index: u32 = 0;
                        if (vm.custom_type_registry.get("IndexError")) |ct| {
                            if (ct.enum_variants) |variants| {
                                for (variants, 0..) |v, i| {
                                    if (std.mem.eql(u8, v.name, "OutOfBounds")) {
                                        variant_index = @intCast(i);
                                        break;
                                    }
                                }
                            }
                        }
                        const err_val = HIRValue{ .enum_variant = .{
                            .type_name = "IndexError",
                            .variant_name = "OutOfBounds",
                            .variant_index = variant_index,
                        } };
                        try vm.stack.push(HIRFrame.initFromHIRValue(err_val));
                        return;
                    }

                    const start_idx = @as(usize, @intCast(start_i));
                    const len = @as(usize, @intCast(len_i));

                    // Bounds check -> return union error variant IndexError.OutOfBounds
                    if (start_idx >= s_val.len or start_idx + len > s_val.len) {
                        var variant_index: u32 = 0;
                        if (vm.custom_type_registry.get("IndexError")) |ct| {
                            if (ct.enum_variants) |variants| {
                                for (variants, 0..) |v, i| {
                                    if (std.mem.eql(u8, v.name, "OutOfBounds")) {
                                        variant_index = @intCast(i);
                                        break;
                                    }
                                }
                            }
                        }
                        const err_val = HIRValue{ .enum_variant = .{
                            .type_name = "IndexError",
                            .variant_name = "OutOfBounds",
                            .variant_index = variant_index,
                        } };
                        try vm.stack.push(HIRFrame.initFromHIRValue(err_val));
                        return;
                    }
                    const end_idx = start_idx + len;

                    // Create substring
                    const substring = s_val[start_idx..end_idx];
                    try vm.stack.push(HIRFrame.initString(substring));
                },
                .Concat => {
                    // Get second string
                    const str2 = try vm.stack.pop();
                    if (str2.value != .string) {
                        return ErrorList.TypeError;
                    }

                    // Concatenate strings
                    const result = try std.fmt.allocPrint(vm.allocator, "{s}{s}", .{ s_val, str2.value.string });
                    try vm.stack.push(HIRFrame.initString(result));
                },
                .Bytes => {
                    const elements = try vm.allocator.alloc(HIRValue, s_val.len);
                    for (s_val, 0..) |byte, i| {
                        elements[i] = HIRValue{ .byte = byte };
                    }
                    const array = HIRValue{ .array = HIRArray{
                        .elements = elements,
                        .element_type = .Byte,
                        .capacity = @intCast(s_val.len),
                    } };

                    try vm.stack.push(HIRFrame.initFromHIRValue(array));
                },
                .ToInt => {
                    const parsed_int = std.fmt.parseInt(i64, s_val, 10) catch {
                        // Return enum variant NumberError.ParseFailed
                        var variant_index: u32 = 0;
                        if (vm.custom_type_registry.get("NumberError")) |ct| {
                            if (ct.enum_variants) |variants| {
                                for (variants, 0..) |v, i| {
                                    if (std.mem.eql(u8, v.name, "ParseFailed")) {
                                        variant_index = @intCast(i);
                                        break;
                                    }
                                }
                            }
                        }
                        const hir = HIRValue{ .enum_variant = .{ .type_name = "NumberError", .variant_name = "ParseFailed", .variant_index = variant_index } };
                        try vm.stack.push(HIRFrame.initFromHIRValue(hir));
                        return; // handled failure path
                    };
                    try vm.stack.push(HIRFrame.initInt(parsed_int));
                },
                .ToString => {
                    // This should never happen since ToString operates on any type, not just strings
                    // But if it does, just return the string as-is
                    try vm.stack.push(HIRFrame.initString(s_val));
                },
                else => return ErrorList.TypeError,
            }
        },
        else => return ErrorList.TypeError,
    }
}

// Helper functions for string operations (used by VM for field access)

/// String concatenation - creates new string from two input strings
pub fn stringConcat(vm: anytype, a: HIRFrame, b: HIRFrame) !HIRFrame {
    const a_str = switch (a.value) {
        .string => |s| s,
        else => return ErrorList.TypeError,
    };

    const b_str = switch (b.value) {
        .string => |s| s,
        else => return ErrorList.TypeError,
    };

    // Allocate new string buffer
    const new_string = try vm.allocator.alloc(u8, a_str.len + b_str.len);
    // Note: This creates a memory leak - in production we'd use string interning
    // TODO: Integrate with memory manager's string interning when available

    @memcpy(new_string[0..a_str.len], a_str);
    @memcpy(new_string[a_str.len..], b_str);

    return HIRFrame.initString(new_string);
}

/// String length operation
pub fn stringLength(vm: anytype, a: HIRFrame) !HIRFrame {
    _ = vm;
    const str = switch (a.value) {
        .string => |s| s,
        else => return ErrorList.TypeError,
    };

    return HIRFrame.initInt(@as(i64, @intCast(str.len)));
}

/// String bytes operation - converts string to array of bytes
pub fn stringBytes(vm: anytype, a: HIRFrame) !HIRFrame {
    const str = switch (a.value) {
        .string => |s| s,
        else => return ErrorList.TypeError,
    };

    // Convert string to array of bytes
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

/// String substring operation - from old VM implementation
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

    // TODO: Use string interning when available
    const new_string = try vm.allocator.dupe(u8, slice);
    return HIRFrame.initString(new_string);
}
