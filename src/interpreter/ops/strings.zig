const std = @import("std");
const HIRValue = @import("../../codegen/hir/soxa_values.zig").HIRValue;
const HIRArray = @import("../../codegen/hir/soxa_values.zig").HIRArray;
const Core = @import("../core.zig");
const HIRFrame = Core.HIRFrame;
const Errors = @import("../../utils/errors.zig");
const ErrorList = Errors.ErrorList;
const ErrorCode = Errors.ErrorCode;

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
                    if (std.meta.eql(arr.elements[logical_len], HIRValue.nothing)) break;
                }
                try vm.stack.push(HIRFrame.initInt(@intCast(logical_len)));
            },
            else => return ErrorList.TypeError,
        }
        return;
    }

    if (s.op == .ToString) {
        const result = try vm.valueToString(val.value);
        try vm.stack.push(HIRFrame.initString(result));
        return;
    }

    if (s.op == .ToInt) {
        const out: HIRValue = switch (val.value) {
            .int => val.value,
            .byte => |u| HIRValue{ .int = @as(i64, u) },
            .float => |f| blk: {
                if (f > @as(f64, @floatFromInt(std.math.maxInt(i64)))) {
                    var variant_index: u32 = 0;
                    if (vm.custom_type_registry.get("ValueError")) |ct| {
                        if (ct.enum_variants) |variants| {
                            for (variants, 0..) |v, i| {
                                if (std.mem.eql(u8, v.name, "Overflow")) {
                                    variant_index = @intCast(i);
                                    break;
                                }
                            }
                        }
                    }
                    break :blk HIRValue{ .enum_variant = .{ .type_name = "ValueError", .variant_name = "Overflow", .variant_index = variant_index } };
                }
                if (f < @as(f64, @floatFromInt(std.math.minInt(i64)))) {
                    var variant_index: u32 = 0;
                    if (vm.custom_type_registry.get("ValueError")) |ct| {
                        if (ct.enum_variants) |variants| {
                            for (variants, 0..) |v, i| {
                                if (std.mem.eql(u8, v.name, "Underflow")) {
                                    variant_index = @intCast(i);
                                    break;
                                }
                            }
                        }
                    }
                    break :blk HIRValue{ .enum_variant = .{ .type_name = "ValueError", .variant_name = "Underflow", .variant_index = variant_index } };
                }
                break :blk HIRValue{ .int = @as(i64, @intFromFloat(f)) };
            },
            .string => |s_val| blk: {
                if (s_val.len > 2 and std.mem.eql(u8, s_val[0..2], "0x")) {
                    const hex_str = s_val[2..];
                    const parsed_hex = std.fmt.parseInt(i64, hex_str, 16) catch {
                        var variant_index: u32 = 0;
                        if (vm.custom_type_registry.get("ValueError")) |ct| {
                            if (ct.enum_variants) |variants| {
                                for (variants, 0..) |v, i| {
                                    if (std.mem.eql(u8, v.name, "ParseFailed")) {
                                        variant_index = @intCast(i);
                                        break;
                                    }
                                }
                            }
                        }
                        break :blk HIRValue{ .enum_variant = .{ .type_name = "ValueError", .variant_name = "ParseFailed", .variant_index = variant_index } };
                    };
                    break :blk HIRValue{ .int = parsed_hex };
                }

                const parsed_int_opt: ?i64 = std.fmt.parseInt(i64, s_val, 10) catch null;
                if (parsed_int_opt) |pi| break :blk HIRValue{ .int = pi };

                const f = std.fmt.parseFloat(f64, s_val) catch {
                    var variant_index: u32 = 0;
                    if (vm.custom_type_registry.get("ValueError")) |ct| {
                        if (ct.enum_variants) |variants| {
                            for (variants, 0..) |v, i| {
                                if (std.mem.eql(u8, v.name, "ParseFailed")) {
                                    variant_index = @intCast(i);
                                    break;
                                }
                            }
                        }
                    }
                    break :blk HIRValue{ .enum_variant = .{ .type_name = "ValueError", .variant_name = "ParseFailed", .variant_index = variant_index } };
                };
                if (f > @as(f64, @floatFromInt(std.math.maxInt(i64)))) {
                    var variant_index: u32 = 0;
                    if (vm.custom_type_registry.get("ValueError")) |ct| {
                        if (ct.enum_variants) |variants| {
                            for (variants, 0..) |v, i| {
                                if (std.mem.eql(u8, v.name, "Overflow")) {
                                    variant_index = @intCast(i);
                                    break;
                                }
                            }
                        }
                    }
                    break :blk HIRValue{ .enum_variant = .{ .type_name = "ValueError", .variant_name = "Overflow", .variant_index = variant_index } };
                }
                if (f < @as(f64, @floatFromInt(std.math.minInt(i64)))) {
                    var variant_index: u32 = 0;
                    if (vm.custom_type_registry.get("ValueError")) |ct| {
                        if (ct.enum_variants) |variants| {
                            for (variants, 0..) |v, i| {
                                if (std.mem.eql(u8, v.name, "Underflow")) {
                                    variant_index = @intCast(i);
                                    break;
                                }
                            }
                        }
                    }
                    break :blk HIRValue{ .enum_variant = .{ .type_name = "ValueError", .variant_name = "Underflow", .variant_index = variant_index } };
                }
                break :blk HIRValue{ .int = @as(i64, @intFromFloat(f)) };
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
                if (s_val.len > 2 and std.mem.eql(u8, s_val[0..2], "0x")) {
                    const hex_str = s_val[2..];
                    const parsed_hex = std.fmt.parseInt(u64, hex_str, 16) catch {
                        var variant_index: u32 = 0;
                        if (vm.custom_type_registry.get("ValueError")) |ct| {
                            if (ct.enum_variants) |variants| {
                                for (variants, 0..) |v, i| {
                                    if (std.mem.eql(u8, v.name, "ParseFailed")) {
                                        variant_index = @intCast(i);
                                        break;
                                    }
                                }
                            }
                        }
                        break :blk HIRValue{ .enum_variant = .{ .type_name = "ValueError", .variant_name = "ParseFailed", .variant_index = variant_index } };
                    };
                    break :blk HIRValue{ .float = @as(f64, @floatFromInt(parsed_hex)) };
                }

                const parsed_ok = std.fmt.parseFloat(f64, s_val) catch {
                    var variant_index: u32 = 0;
                    if (vm.custom_type_registry.get("ValueError")) |ct| {
                        if (ct.enum_variants) |variants| {
                            for (variants, 0..) |v, i| {
                                if (std.mem.eql(u8, v.name, "ParseFailed")) {
                                    variant_index = @intCast(i);
                                    break;
                                }
                            }
                        }
                    }
                    break :blk HIRValue{ .enum_variant = .{ .type_name = "ValueError", .variant_name = "ParseFailed", .variant_index = variant_index } };
                };
                break :blk HIRValue{ .float = parsed_ok };
            },
            else => HIRValue{ .float = 0.0 },
        };
        try vm.stack.push(HIRFrame.initFromHIRValue(out));
        return;
    }
    if (s.op == .ToByte) {
        const out: HIRValue = switch (val.value) {
            .byte => |b| HIRValue{ .byte = b },
            .int => |i| blk: {
                if (i >= 0 and i <= 255) {
                    break :blk HIRValue{ .byte = @intCast(i) };
                }
                var variant_index: u32 = 0;
                if (vm.custom_type_registry.get("ValueError")) |ct| {
                    if (ct.enum_variants) |variants| {
                        for (variants, 0..) |v, idx| {
                            if (std.mem.eql(u8, v.name, "OutOfBounds")) {
                                variant_index = @intCast(idx);
                                break;
                            }
                        }
                    }
                }
                const error_variant = HIRValue{ .enum_variant = .{ .type_name = "ValueError", .variant_name = "OutOfBounds", .variant_index = variant_index, .path = null } };
                break :blk error_variant;
            },
            .float => |f| blk: {
                const i: i64 = @as(i64, @intFromFloat(f));
                if (i >= 0 and i <= 255) {
                    break :blk HIRValue{ .byte = @intCast(i) };
                }
                var variant_index: u32 = 0;
                if (vm.custom_type_registry.get("ValueError")) |ct| {
                    if (ct.enum_variants) |variants| {
                        for (variants, 0..) |v, idx| {
                            if (std.mem.eql(u8, v.name, "OutOfBounds")) {
                                variant_index = @intCast(idx);
                                break;
                            }
                        }
                    }
                }
                const error_variant = HIRValue{ .enum_variant = .{ .type_name = "ValueError", .variant_name = "OutOfBounds", .variant_index = variant_index, .path = null } };
                break :blk error_variant;
            },
            .string => |s_val| blk: {
                if (s_val.len == 0) {
                    var v_idx: u32 = 0;
                    if (vm.custom_type_registry.get("ValueError")) |ct| {
                        if (ct.enum_variants) |variants| {
                            for (variants, 0..) |v, i| {
                                if (std.mem.eql(u8, v.name, "ParseFailed")) {
                                    v_idx = @intCast(i);
                                    break;
                                }
                            }
                        }
                    }
                    const parse_failed_variant = HIRValue{ .enum_variant = .{ .type_name = "ValueError", .variant_name = "ParseFailed", .variant_index = v_idx, .path = null } };
                    break :blk parse_failed_variant;
                }

                if (s_val.len == 1) {
                    break :blk HIRValue{ .byte = s_val[0] };
                }

                if (s_val.len > 2 and std.mem.eql(u8, s_val[0..2], "0x")) {
                    const hex_str = s_val[2..];
                    const parsed_hex_byte = std.fmt.parseInt(u8, hex_str, 16) catch {
                        var v_idx: u32 = 0;
                        if (vm.custom_type_registry.get("ValueError")) |ct| {
                            if (ct.enum_variants) |variants| {
                                for (variants, 0..) |v, i| {
                                    if (std.mem.eql(u8, v.name, "ParseFailed")) {
                                        v_idx = @intCast(i);
                                        break;
                                    }
                                }
                            }
                        }
                        const parse_failed_variant = HIRValue{ .enum_variant = .{ .type_name = "ValueError", .variant_name = "ParseFailed", .variant_index = v_idx, .path = null } };
                        break :blk parse_failed_variant;
                    };
                    break :blk HIRValue{ .byte = parsed_hex_byte };
                }

                const parsed_opt: ?i64 = std.fmt.parseInt(i64, s_val, 10) catch null;
                if (parsed_opt) |parsed_int| {
                    if (parsed_int >= 0 and parsed_int <= 255) {
                        break :blk HIRValue{ .byte = @intCast(parsed_int) };
                    }
                    var v_idx2: u32 = 0;
                    if (vm.custom_type_registry.get("ValueError")) |ct2| {
                        if (ct2.enum_variants) |variants| {
                            for (variants, 0..) |v, i| {
                                if (std.mem.eql(u8, v.name, "OutOfBounds")) {
                                    v_idx2 = @intCast(i);
                                    break;
                                }
                            }
                        }
                    }
                    break :blk HIRValue{ .enum_variant = .{ .type_name = "ValueError", .variant_name = "OutOfBounds", .variant_index = v_idx2, .path = null } };
                }

                const f = std.fmt.parseFloat(f64, s_val) catch {
                    var v_idx_pf: u32 = 0;
                    if (vm.custom_type_registry.get("ValueError")) |ct| {
                        if (ct.enum_variants) |variants| {
                            for (variants, 0..) |v, i| {
                                if (std.mem.eql(u8, v.name, "ParseFailed")) {
                                    v_idx_pf = @intCast(i);
                                    break;
                                }
                            }
                        }
                    }
                    break :blk HIRValue{ .enum_variant = .{ .type_name = "ValueError", .variant_name = "ParseFailed", .variant_index = v_idx_pf, .path = null } };
                };
                const floored: i64 = @as(i64, @intFromFloat(f));
                if (floored >= 0 and floored <= 255) {
                    break :blk HIRValue{ .byte = @intCast(floored) };
                }
                var v_idx2: u32 = 0;
                if (vm.custom_type_registry.get("ValueError")) |ct2| {
                    if (ct2.enum_variants) |variants| {
                        for (variants, 0..) |v, i| {
                            if (std.mem.eql(u8, v.name, "OutOfBounds")) {
                                v_idx2 = @intCast(i);
                                break;
                            }
                        }
                    }
                }
                break :blk HIRValue{ .enum_variant = .{ .type_name = "ValueError", .variant_name = "OutOfBounds", .variant_index = v_idx2, .path = null } };
            },
            else => blk_unsupported: {
                var v_idx: u32 = 0;
                if (vm.custom_type_registry.get("ValueError")) |ct| {
                    if (ct.enum_variants) |variants| {
                        for (variants, 0..) |v, i| {
                            if (std.mem.eql(u8, v.name, "ParseFailed")) {
                                v_idx = @intCast(i);
                                break;
                            }
                        }
                    }
                }
                const unsupported_variant = HIRValue{ .enum_variant = .{ .type_name = "ValueError", .variant_name = "ParseFailed", .variant_index = v_idx, .path = null } };
                break :blk_unsupported unsupported_variant;
            },
        };
        try vm.stack.push(HIRFrame.initFromHIRValue(out));
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
                    const parsed_int = std.fmt.parseInt(i64, s_val, 10) catch {
                        var variant_index: u32 = 0;
                        if (vm.custom_type_registry.get("ValueError")) |ct| {
                            if (ct.enum_variants) |variants| {
                                for (variants, 0..) |v, i| {
                                    if (std.mem.eql(u8, v.name, "ParseFailed")) {
                                        variant_index = @intCast(i);
                                        break;
                                    }
                                }
                            }
                        }
                        const hir = HIRValue{ .enum_variant = .{ .type_name = "ValueError", .variant_name = "ParseFailed", .variant_index = variant_index } };
                        try vm.stack.push(HIRFrame.initFromHIRValue(hir));
                        return;
                    };
                    try vm.stack.push(HIRFrame.initInt(parsed_int));
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
