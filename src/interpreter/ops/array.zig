const std = @import("std");
const SoxaValues = @import("../../codegen/hir/soxa_values.zig");
const HIRValue = SoxaValues.HIRValue;
const nothing_value = @import("../../codegen/hir/soxa_values.zig").nothing_value;
const HIRStructField = SoxaValues.HIRStructField;
const HIRMapEntry = SoxaValues.HIRMapEntry;
const HIRArray = SoxaValues.HIRArray;
const SoxaTypes = @import("../../codegen/hir/soxa_types.zig");
const HIRType = SoxaTypes.HIRType;
const ArrayStorageKind = SoxaTypes.ArrayStorageKind;
const Core = @import("../core.zig");
const HIRFrame = Core.HIRFrame;
const Errors = @import("../../utils/errors.zig");
const ErrorList = Errors.ErrorList;
const ErrorCode = Errors.ErrorCode;

fn ensureDynamicArrayStorage(vm: anytype, array: HIRArray) !HIRArray {
    if (array.storage_kind == .dynamic) return array;

    var converted = array;
    const element_count = array.elements.len;
    const new_elements = try vm.allocator.alloc(HIRValue, element_count);

    var i: usize = 0;
    while (i < element_count) : (i += 1) {
        new_elements[i] = try vm.deepCopyValueToAllocator(vm.allocator, &array.elements[i]);
    }

    converted.elements = new_elements;
    converted.capacity = @intCast(element_count);
    converted.storage_kind = .dynamic;
    return converted;
}

pub fn exec(vm: anytype, instruction: anytype) !void {
    if (@hasField(@TypeOf(instruction), "ArrayNew")) {
        try arrayNew(vm, instruction.ArrayNew);
    } else if (@hasField(@TypeOf(instruction), "ArrayGet")) {
        try arrayGet(vm, instruction.ArrayGet);
    } else if (@hasField(@TypeOf(instruction), "ArraySet")) {
        try arraySet(vm, instruction.ArraySet);
    } else if (@hasField(@TypeOf(instruction), "ArrayPush")) {
        try arrayPush(vm);
    } else if (@hasField(@TypeOf(instruction), "ArrayPop")) {
        try arrayPop(vm);
    } else if (@hasField(@TypeOf(instruction), "ArrayInsert")) {
        try arrayInsert(vm);
    } else if (@hasField(@TypeOf(instruction), "ArrayRemove")) {
        try arrayRemove(vm);
    } else if (@hasField(@TypeOf(instruction), "ArraySlice")) {
        try arraySlice(vm);
    } else if (@hasField(@TypeOf(instruction), "ArrayLen")) {
        try arrayLen(vm);
    } else if (@hasField(@TypeOf(instruction), "ArrayConcat")) {
        try arrayConcat(vm);
    } else if (@hasField(@TypeOf(instruction), "Range")) {
        try arrayRange(vm, instruction.Range);
    } else if (@hasField(@TypeOf(instruction), "ArrayGetAndAdd")) {
        try arrayGetAndAdd(vm, instruction.ArrayGetAndAdd);
    } else if (@hasField(@TypeOf(instruction), "ArrayGetAndSub")) {
        try arrayGetAndSub(vm, instruction.ArrayGetAndSub);
    } else if (@hasField(@TypeOf(instruction), "ArrayGetAndMul")) {
        try arrayGetAndMul(vm, instruction.ArrayGetAndMul);
    } else if (@hasField(@TypeOf(instruction), "ArrayGetAndDiv")) {
        try arrayGetAndDiv(vm, instruction.ArrayGetAndDiv);
    } else if (@hasField(@TypeOf(instruction), "ArrayGetAndMod")) {
        try arrayGetAndMod(vm, instruction.ArrayGetAndMod);
    } else if (@hasField(@TypeOf(instruction), "ArrayGetAndPow")) {
        try arrayGetAndPow(vm, instruction.ArrayGetAndPow);
    } else {
        unreachable;
    }
}

fn arrayNew(vm: anytype, a: anytype) !void {
    const storage_kind: ArrayStorageKind = if (@hasField(@TypeOf(a), "storage_kind"))
        a.storage_kind
    else
        .dynamic;
    const requested_len: usize = @intCast(a.size);
    const initial_capacity: usize = switch (storage_kind) {
        .dynamic => if (requested_len == 0) 8 else requested_len,
        else => requested_len,
    };
    const elements = try vm.allocator.alloc(HIRValue, initial_capacity);

    const default_value = getDefaultValue(a.element_type);

    var i: usize = 0;
    while (i < requested_len and i < elements.len) : (i += 1) {
        elements[i] = default_value;
    }
    while (i < elements.len) : (i += 1) {
        elements[i] = nothing_value;
    }

    const new_array = HIRValue{
        .array = .{
            .elements = elements,
            .capacity = @intCast(initial_capacity),
            .element_type = a.element_type,
            .nested_element_type = a.nested_element_type,
            .storage_kind = storage_kind,
        },
    };

    try vm.stack.push(HIRFrame.initFromHIRValue(new_array));
}

fn arrayGet(vm: anytype, a: anytype) !void {
    if (vm.stack.sp < 2) {
        return ErrorList.StackUnderflow;
    }
    const index = try vm.stack.pop();
    const array = try vm.stack.pop();

    const index_val = switch (index.value) {
        .int => |i| if (i < 0) {
            return vm.reporter.reportRuntimeError(null, ErrorCode.VARIABLE_NOT_FOUND, "Array index cannot be negative: {}", .{i});
        } else @as(u32, @intCast(i)),
        .byte => |u| @as(u32, u),
        .tetra => |t| @as(u32, t),
        .string => |s| blk: {
            const parsed = std.fmt.parseInt(i64, s, 10) catch {
                try vm.stack.push(array);
                return;
            };
            if (parsed < 0) {
                return vm.reporter.reportRuntimeError(null, ErrorCode.VARIABLE_NOT_FOUND, "Array index cannot be negative: {}", .{parsed});
            }
            break :blk @as(u32, @intCast(parsed));
        },
        .array => |arr| blk: {
            if (arr.elements.len > 0) {
                break :blk switch (arr.elements[0]) {
                    .int => |i| if (i < 0) {
                        return vm.reporter.reportRuntimeError(null, ErrorCode.VARIABLE_NOT_FOUND, "Array index cannot be negative: {}", .{i});
                    } else @as(u32, @intCast(i)),
                    .byte => |u| @as(u32, u),
                    .tetra => |t| @as(u32, t),
                    else => {
                        try vm.stack.push(array);
                        return;
                    },
                };
            } else {
                try vm.stack.push(array);
                return;
            }
        },
        .nothing => {
            try vm.stack.push(array);
            return;
        },
        else => {
            try vm.stack.push(array);
            return;
        },
    };

    switch (array.value) {
        .array => |arr| {
            var actual_length: u32 = 0;
            for (arr.elements) |elem| {
                if (elem == .nothing) break;
                actual_length += 1;
            }

            if (a.bounds_check and index_val >= actual_length) {
                return ErrorList.IndexOutOfBounds;
            }

            const element = if (index_val < actual_length)
                arr.elements[index_val]
            else
                getDefaultValue(arr.element_type);

            if (element == .nothing and arr.element_type != .Nothing and arr.element_type != .Unknown) {
                try vm.stack.push(HIRFrame.initFromHIRValue(getDefaultValue(arr.element_type)));
                return;
            }
            try vm.stack.push(HIRFrame.initFromHIRValue(element));
        },
        .string => |s| {
            if (index_val >= s.len) {
                try vm.stack.push(HIRFrame.initFromHIRValue(nothing_value));
                return;
            }
            const char_str = s[index_val .. index_val + 1];
            try vm.stack.push(HIRFrame.initFromHIRValue(HIRValue{ .string = char_str }));
        },
        .nothing => {
            try vm.stack.push(HIRFrame.initFromHIRValue(nothing_value));
        },
        else => {
            return vm.reporter.reportRuntimeError(null, ErrorCode.VARIABLE_NOT_FOUND, "Cannot index non-array value: {s}", .{@tagName(array.value)});
        },
    }
}

fn arraySet(vm: anytype, a: anytype) !void {
    _ = a;
    if (vm.stack.sp < 3) {
        return ErrorList.StackUnderflow;
    }
    const value = try vm.stack.pop();
    const index = try vm.stack.pop();
    const array_frame = try vm.stack.pop();

    const index_val = switch (index.value) {
        .int => |i| if (i < 0) {
            return vm.reporter.reportRuntimeError(null, ErrorCode.VARIABLE_NOT_FOUND, "Array index cannot be negative: {}", .{i});
        } else @as(u32, @intCast(i)),
        .byte => |u| @as(u32, u),
        .tetra => |t| @as(u32, t),
        .string => |s| blk: {
            const parsed = std.fmt.parseInt(i64, s, 10) catch {
                try vm.stack.push(array_frame); // Push original array back
                return;
            };
            if (parsed < 0) {
                return vm.reporter.reportRuntimeError(null, ErrorCode.VARIABLE_NOT_FOUND, "Array index cannot be negative: {}", .{parsed});
            }
            break :blk @as(u32, @intCast(parsed));
        },
        .nothing => {
            return vm.reporter.reportRuntimeError(null, ErrorCode.VARIABLE_NOT_FOUND, "Array index cannot be nothing", .{});
        },
        else => {
            return vm.reporter.reportRuntimeError(null, ErrorCode.VARIABLE_NOT_FOUND, "Invalid array index type: {s}", .{@tagName(index.value)});
        },
    };

    switch (array_frame.value) {
        .array => |arr| {
            var mutable_arr = arr;

            if (index_val >= mutable_arr.capacity) {
                mutable_arr = try ensureDynamicArrayStorage(vm, mutable_arr);
                const new_capacity = @max(mutable_arr.capacity * 2, index_val + 1);
                const new_elements = try vm.allocator.realloc(mutable_arr.elements, new_capacity);
                mutable_arr.elements = new_elements;
                mutable_arr.capacity = new_capacity;

                for (mutable_arr.elements[arr.capacity..new_capacity]) |*element| {
                    element.* = nothing_value;
                }
            }

            mutable_arr.elements[index_val] = value.value;

            const updated_array = HIRValue{ .array = mutable_arr };
            try vm.stack.push(HIRFrame.initFromHIRValue(updated_array));
        },
        else => {
            return vm.reporter.reportRuntimeError(null, ErrorCode.VARIABLE_NOT_FOUND, "Cannot set element in non-array value: {s}", .{@tagName(array_frame.value)});
        },
    }
}

fn arrayPush(vm: anytype) !void {
    const value = try vm.stack.pop();
    try handleArrayPush(vm, value.value);
}

fn arrayPop(vm: anytype) !void {
    const array = try vm.stack.pop();

    switch (array.value) {
        .array => |arr| {
            const mutable_arr = arr;

            var last_index: ?u32 = null;
            for (arr.elements, 0..) |elem, i| {
                if (elem == .nothing) break;
                last_index = @intCast(i);
            }

            if (last_index) |idx| {
                const last_element = arr.elements[idx];

                mutable_arr.elements[idx] = nothing_value;

                const updated_array = HIRValue{ .array = mutable_arr };
                try vm.stack.push(HIRFrame.initFromHIRValue(updated_array));

                try vm.stack.push(HIRFrame.initFromHIRValue(last_element));
            } else {
                return vm.reporter.reportRuntimeError(null, ErrorCode.VARIABLE_NOT_FOUND, "Cannot pop from empty array", .{});
            }
        },
        .string => |s_val| {
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
                    // this is to handle multi-byte sequences
                    last_char_start = i;
                    break;
                }
            }

            const last_char = s_val[last_char_start..s_val.len];
            const remaining = s_val[0..last_char_start];

            try vm.stack.push(HIRFrame.initString(remaining));
            try vm.stack.push(HIRFrame.initString(last_char));
        },
        else => {
            return vm.reporter.reportRuntimeError(null, ErrorCode.VARIABLE_NOT_FOUND, "Cannot pop from non-array value: {s}", .{@tagName(array.value)});
        },
    }
}

fn arrayInsert(vm: anytype) !void {
    const value = try vm.stack.pop();
    const index_frame = try vm.stack.pop();
    const container = try vm.stack.pop();

    const index_val = switch (index_frame.value) {
        .int => |i| if (i < 0) {
            return vm.reporter.reportRuntimeError(null, ErrorCode.VARIABLE_NOT_FOUND, "Array index cannot be negative: {}", .{i});
        } else @as(u32, @intCast(i)),
        .byte => |u| @as(u32, u),
        .tetra => |t| @as(u32, t),
        .string => |s| blk: {
            const parsed = std.fmt.parseInt(i64, s, 10) catch {
                return vm.reporter.reportRuntimeError(null, ErrorCode.VARIABLE_NOT_FOUND, "Invalid array index: {s}", .{s});
            };
            if (parsed < 0) {
                return vm.reporter.reportRuntimeError(null, ErrorCode.VARIABLE_NOT_FOUND, "Array index cannot be negative: {}", .{parsed});
            }
            break :blk @as(u32, @intCast(parsed));
        },
        .nothing => {
            try vm.stack.push(container);
            return vm.reporter.reportRuntimeError(null, ErrorCode.VARIABLE_NOT_FOUND, "Array index cannot be nothing", .{});
        },
        else => {
            return vm.reporter.reportRuntimeError(null, ErrorCode.VARIABLE_NOT_FOUND, "Invalid array index type: {s}", .{@tagName(index_frame.value)});
        },
    };

    switch (container.value) {
        .array => |arr| {
            var logical_len: u32 = 0;
            for (arr.elements) |elem| {
                if (elem == .nothing) break;
                logical_len += 1;
            }

            if (index_val > logical_len) {
                return vm.reporter.reportRuntimeError(null, ErrorCode.VARIABLE_NOT_FOUND, "Array index out of bounds: {} (length: {})", .{ index_val, logical_len });
            }

            var mutable_arr = arr;
            mutable_arr = try ensureDynamicArrayStorage(vm, mutable_arr);

            if (mutable_arr.element_type == .Unknown or mutable_arr.element_type == .Nothing) {
                mutable_arr.element_type = switch (value.value) {
                    .int => .Int,
                    .byte => .Byte,
                    .float => .Float,
                    .string => .String,
                    .tetra => .Tetra,
                    .array => HIRType.Unknown,
                    .struct_instance => HIRType.Unknown,
                    else => mutable_arr.element_type,
                };
            }

            if (logical_len >= mutable_arr.capacity) {
                const new_capacity = @max(mutable_arr.capacity * 2, logical_len + 1);
                const new_elements = try vm.allocator.realloc(mutable_arr.elements, new_capacity);
                for (new_elements[mutable_arr.capacity..new_capacity]) |*e| e.* = nothing_value;
                mutable_arr.elements = new_elements;
                mutable_arr.capacity = new_capacity;
            }

            var i: i32 = @as(i32, @intCast(logical_len));
            while (i >= @as(i32, @intCast(index_val))) : (i -= 1) {
                const from_idx: usize = @intCast(i);
                const to_idx: usize = from_idx + 1;
                mutable_arr.elements[to_idx] = if (from_idx < mutable_arr.capacity) mutable_arr.elements[from_idx] else nothing_value;
            }

            mutable_arr.elements[index_val] = value.value;

            try vm.stack.push(HIRFrame.initFromHIRValue(HIRValue{ .array = mutable_arr }));
        },
        .string => |s| {
            if (index_val > s.len) {
                return vm.reporter.reportRuntimeError(null, ErrorCode.VARIABLE_NOT_FOUND, "String index out of bounds: {} (length: {})", .{ index_val, s.len });
            }
            const insert_str = switch (value.value) {
                .string => |sv| sv,
                else => {
                    return vm.reporter.reportRuntimeError(null, ErrorCode.VARIABLE_NOT_FOUND, "Cannot insert non-string value into string: {s}", .{@tagName(value.value)});
                },
            };
            const idx: usize = index_val;
            const new_len = s.len + insert_str.len;
            const new_buf = try vm.allocator.alloc(u8, new_len);
            @memcpy(new_buf[0..idx], s[0..idx]);
            @memcpy(new_buf[idx .. idx + insert_str.len], insert_str);
            @memcpy(new_buf[idx + insert_str.len ..], s[idx..]);
            try vm.stack.push(HIRFrame.initString(new_buf));
        },
        else => {
            return vm.reporter.reportRuntimeError(null, ErrorCode.VARIABLE_NOT_FOUND, "Cannot insert into non-array value: {s}", .{@tagName(container.value)});
        },
    }
}

fn arrayRemove(vm: anytype) !void {
    const index_frame = try vm.stack.pop();
    const container = try vm.stack.pop();

    const index_val = switch (index_frame.value) {
        .int => |i| if (i < 0) {
            return vm.reporter.reportRuntimeError(null, ErrorCode.VARIABLE_NOT_FOUND, "Array index cannot be negative: {}", .{i});
        } else @as(u32, @intCast(i)),
        .byte => |u| @as(u32, u),
        .tetra => |t| @as(u32, t),
        .string => |s| blk: {
            const parsed = std.fmt.parseInt(i64, s, 10) catch {
                return vm.reporter.reportRuntimeError(null, ErrorCode.VARIABLE_NOT_FOUND, "Invalid array index: {s}", .{s});
            };
            if (parsed < 0) {
                return vm.reporter.reportRuntimeError(null, ErrorCode.VARIABLE_NOT_FOUND, "Array index cannot be negative: {}", .{parsed});
            }
            break :blk @as(u32, @intCast(parsed));
        },
        .nothing => {
            try vm.stack.push(container); // Push original array back
            return vm.reporter.reportRuntimeError(null, ErrorCode.VARIABLE_NOT_FOUND, "Array index cannot be nothing", .{});
        },
        else => {
            return vm.reporter.reportRuntimeError(null, ErrorCode.VARIABLE_NOT_FOUND, "Invalid array index type: {s}", .{@tagName(index_frame.value)});
        },
    };

    switch (container.value) {
        .array => |arr| {
            var logical_len: u32 = 0;
            for (arr.elements) |elem| {
                if (elem == .nothing) break;
                logical_len += 1;
            }
            if (index_val >= logical_len) {
                return vm.reporter.reportRuntimeError(null, ErrorCode.VARIABLE_NOT_FOUND, "Array index out of bounds: {} (length: {})", .{ index_val, logical_len });
            }
            var mutable_arr = arr;
            mutable_arr = try ensureDynamicArrayStorage(vm, mutable_arr);
            const removed_elem = mutable_arr.elements[index_val];
            var i: u32 = index_val;
            while (i + 1 < mutable_arr.capacity and i + 1 < logical_len) : (i += 1) {
                mutable_arr.elements[i] = mutable_arr.elements[i + 1];
            }
            mutable_arr.elements[logical_len - 1] = nothing_value;

            try vm.stack.push(HIRFrame.initFromHIRValue(HIRValue{ .array = mutable_arr }));
            try vm.stack.push(HIRFrame.initFromHIRValue(removed_elem));
        },
        .string => |s| {
            if (index_val >= s.len) {
                return vm.reporter.reportRuntimeError(null, ErrorCode.VARIABLE_NOT_FOUND, "String index out of bounds: {} (length: {})", .{ index_val, s.len });
            }
            const idx: usize = index_val;
            const removed_slice = s[idx .. idx + 1];
            const new_len = s.len - 1;
            const new_buf = try vm.allocator.alloc(u8, new_len);
            @memcpy(new_buf[0..idx], s[0..idx]);
            @memcpy(new_buf[idx..], s[idx + 1 ..]);
            try vm.stack.push(HIRFrame.initString(new_buf));
            try vm.stack.push(HIRFrame.initString(removed_slice));
        },
        else => {
            return vm.reporter.reportRuntimeError(null, ErrorCode.VARIABLE_NOT_FOUND, "Cannot remove from non-array value: {s}", .{@tagName(container.value)});
        },
    }
}

fn arraySlice(vm: anytype) !void {
    const length_frame = try vm.stack.pop();
    const start_frame = try vm.stack.pop();
    const container = try vm.stack.pop();

    const start_val = switch (start_frame.value) {
        .int => |i| if (i < 0) {
            return vm.reporter.reportRuntimeError(null, ErrorCode.VARIABLE_NOT_FOUND, "Array start index cannot be negative: {}", .{i});
        } else @as(u32, @intCast(i)),
        .byte => |u| @as(u32, u),
        .tetra => |t| @as(u32, t),
        .string => |s| blk: {
            const parsed = std.fmt.parseInt(i64, s, 10) catch {
                return vm.reporter.reportRuntimeError(null, ErrorCode.VARIABLE_NOT_FOUND, "Invalid array start index: {s}", .{s});
            };
            if (parsed < 0) {
                return vm.reporter.reportRuntimeError(null, ErrorCode.VARIABLE_NOT_FOUND, "Array start index cannot be negative: {}", .{parsed});
            }
            break :blk @as(u32, @intCast(parsed));
        },
        .nothing => {
            return vm.reporter.reportRuntimeError(null, ErrorCode.VARIABLE_NOT_FOUND, "Array start index cannot be nothing", .{});
        },
        else => {
            return vm.reporter.reportRuntimeError(null, ErrorCode.VARIABLE_NOT_FOUND, "Invalid array start index type: {s}", .{@tagName(start_frame.value)});
        },
    };

    const length_val = switch (length_frame.value) {
        .int => |i| if (i < 0) {
            return vm.reporter.reportRuntimeError(null, ErrorCode.VARIABLE_NOT_FOUND, "Array length cannot be negative: {}", .{i});
        } else @as(u32, @intCast(i)),
        .byte => |u| @as(u32, u),
        .tetra => |t| @as(u32, t),
        .string => |s| blk: {
            const parsed = std.fmt.parseInt(i64, s, 10) catch {
                return vm.reporter.reportRuntimeError(null, ErrorCode.VARIABLE_NOT_FOUND, "Invalid array length: {s}", .{s});
            };
            if (parsed < 0) {
                return vm.reporter.reportRuntimeError(null, ErrorCode.VARIABLE_NOT_FOUND, "Array length cannot be negative: {}", .{parsed});
            }
            break :blk @as(u32, @intCast(parsed));
        },
        .nothing => {
            return vm.reporter.reportRuntimeError(null, ErrorCode.VARIABLE_NOT_FOUND, "Array length cannot be nothing", .{});
        },
        else => {
            return vm.reporter.reportRuntimeError(null, ErrorCode.VARIABLE_NOT_FOUND, "Invalid array length type: {s}", .{@tagName(length_frame.value)});
        },
    };

    switch (container.value) {
        .array => |arr| {
            var logical_len: u32 = 0;
            for (arr.elements) |elem| {
                if (elem == .nothing) break;
                logical_len += 1;
            }

            if (start_val > logical_len) {
                return vm.reporter.reportRuntimeError(null, ErrorCode.VARIABLE_NOT_FOUND, "Array start index out of bounds: {} (length: {})", .{ start_val, logical_len });
            }
            if (start_val + length_val > logical_len) {
                return vm.reporter.reportRuntimeError(null, ErrorCode.VARIABLE_NOT_FOUND, "Array slice out of bounds: start={}, length={} (max length: {})", .{ start_val, length_val, logical_len - start_val });
            }

            const sliced_elements = try vm.allocator.alloc(HIRValue, length_val);
            for (0..length_val) |i| {
                sliced_elements[i] = arr.elements[start_val + i];
            }

            const sliced_arr = HIRValue{ .array = .{
                .elements = sliced_elements,
                .capacity = length_val,
                .element_type = arr.element_type,
            } };

            try vm.stack.push(HIRFrame.initFromHIRValue(sliced_arr));
        },
        .string => |s| {
            if (start_val > s.len) {
                return vm.reporter.reportRuntimeError(null, ErrorCode.VARIABLE_NOT_FOUND, "String start index out of bounds: {} (length: {})", .{ start_val, s.len });
            }
            if (start_val + length_val > s.len) {
                return vm.reporter.reportRuntimeError(null, ErrorCode.VARIABLE_NOT_FOUND, "String slice out of bounds: start={}, length={} (max length: {})", .{ start_val, length_val, s.len - start_val });
            }

            const sliced_str = s[start_val .. start_val + length_val];
            try vm.stack.push(HIRFrame.initString(sliced_str));
        },
        else => {
            return vm.reporter.reportRuntimeError(null, ErrorCode.VARIABLE_NOT_FOUND, "Cannot slice non-array/non-string value: {s}", .{@tagName(container.value)});
        },
    }
}

fn arrayLen(vm: anytype) !void {
    const value = try vm.stack.pop();
    switch (value.value) {
        .array => |arr| {
            var length: u32 = 0;
            for (arr.elements) |elem| {
                if (elem == .nothing) break;
                length += 1;
            }
            try vm.stack.push(HIRFrame.initInt(@as(i64, @intCast(length))));
        },
        .string => |s| {
            try vm.stack.push(HIRFrame.initInt(@as(i64, @intCast(s.len))));
        },
        else => {
            return vm.reporter.reportRuntimeError(null, ErrorCode.VARIABLE_NOT_FOUND, "Cannot get length of non-array/non-string value: {s}", .{@tagName(value.value)});
        },
    }
}

fn arrayConcat(vm: anytype) !void {
    const b = try vm.stack.pop();
    const a = try vm.stack.pop();

    switch (a.value) {
        .array => |arr_a| {
            switch (b.value) {
                .array => |arr_b| {
                    // Calculate lengths
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

                    const new_elements = try vm.allocator.alloc(HIRValue, len_a + len_b);

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
                },
                else => {
                    return vm.reporter.reportRuntimeError(null, ErrorCode.VARIABLE_NOT_FOUND, "Cannot concatenate array with {s}", .{@tagName(b.value)});
                },
            }
        },
        else => {
            return vm.reporter.reportRuntimeError(null, ErrorCode.VARIABLE_NOT_FOUND, "Cannot concatenate {s} with array", .{@tagName(a.value)});
        },
    }
}

fn handleArrayPush(vm: anytype, element_value: HIRValue) !void {
    const array_frame = try vm.stack.pop();
    switch (array_frame.value) {
        .array => |arr| {
            var mutable_arr = arr;
            mutable_arr = try ensureDynamicArrayStorage(vm, mutable_arr);

            if (mutable_arr.element_type == .Unknown or mutable_arr.element_type == .Nothing) {
                mutable_arr.element_type = switch (element_value) {
                    .int => .Int,
                    .byte => .Byte,
                    .float => .Float,
                    .string => .String,
                    .tetra => .Tetra,
                    .array => HIRType.Unknown,
                    .struct_instance => HIRType.Unknown,
                    else => mutable_arr.element_type,
                };
            }

            var insert_index: u32 = 0;
            for (mutable_arr.elements, 0..) |elem, i| {
                if (elem == .nothing) {
                    insert_index = @intCast(i);
                    break;
                }
                insert_index = @intCast(i + 1);
            }

            if (insert_index >= mutable_arr.capacity) {
                const new_capacity = @max(mutable_arr.capacity * 2, insert_index + 1);
                const new_elements = try vm.allocator.realloc(mutable_arr.elements, new_capacity);
                mutable_arr.elements = new_elements;
                mutable_arr.capacity = new_capacity;

                for (mutable_arr.elements[arr.capacity..new_capacity]) |*element| {
                    element.* = nothing_value;
                }
            }

            mutable_arr.elements[insert_index] = element_value;

            const updated_array = HIRValue{ .array = mutable_arr };
            try vm.stack.push(HIRFrame.initFromHIRValue(updated_array));
        },
        else => {
            return vm.reporter.reportRuntimeError(null, ErrorCode.VARIABLE_NOT_FOUND, "Cannot push to non-array value: {s}", .{@tagName(array_frame.value)});
        },
    }
}

fn arrayRange(vm: anytype, r: anytype) !void {
    const end_frame = try vm.stack.pop();
    const start_frame = try vm.stack.pop();

    const start_val = switch (start_frame.value) {
        .int => |i| i,
        .byte => |b| @as(i32, @intCast(b)),
        .float => |f| @as(i32, @intCast(@as(i64, @intFromFloat(f)))),
        else => return vm.reporter.reportRuntimeError(null, ErrorCode.VARIABLE_NOT_FOUND, "Range start must be numeric, got {s}", .{@tagName(start_frame.value)}),
    };

    const end_val = switch (end_frame.value) {
        .int => |i| i,
        .byte => |b| @as(i32, @intCast(b)),
        .float => |f| @as(i32, @intCast(@as(i64, @intFromFloat(f)))),
        else => return vm.reporter.reportRuntimeError(null, ErrorCode.VARIABLE_NOT_FOUND, "Range end must be numeric, got {s}", .{@tagName(end_frame.value)}),
    };

    const size = if (end_val >= start_val)
        @as(u32, @intCast(end_val - start_val + 1))
    else
        0;

    const elements = try vm.allocator.alloc(HIRValue, size);

    var i: u32 = 0;
    var current = start_val;
    while (i < size) : (i += 1) {
        elements[i] = HIRValue{ .int = current };
        current += 1;
    }

    const range_array = HIRValue{
        .array = .{
            .elements = elements,
            .capacity = size,
            .element_type = r.element_type,
        },
    };

    try vm.stack.push(HIRFrame.initFromHIRValue(range_array));
}

fn getDefaultValue(hir_type: HIRType) HIRValue {
    return switch (hir_type) {
        .Int => HIRValue{ .int = 0 },
        .Byte => HIRValue{ .byte = 0 },
        .Float => HIRValue{ .float = 0.0 },
        .String => HIRValue{ .string = "" },
        .Tetra => HIRValue{ .tetra = 0 },
        .Nothing => nothing_value,
        .Poison => nothing_value,
        .Array => HIRValue{ .array = .{
            .elements = &[_]HIRValue{},
            .element_type = .Unknown,
            .capacity = 0,
        } },
        .Struct => HIRValue{ .struct_instance = .{
            .type_name = "",
            .fields = &[_]HIRStructField{},
        } },
        .Map => HIRValue{ .map = .{
            .entries = &[_]HIRMapEntry{},
            .key_type = .Unknown,
            .value_type = .Unknown,
        } },
        .Enum => HIRValue{ .enum_variant = .{
            .type_name = "",
            .variant_name = "",
            .variant_index = 0,
        } },
        .Function => nothing_value,
        .Union => nothing_value,
        .Unknown => nothing_value,
    };
}

fn arrayGetAndAdd(vm: anytype, a: anytype) !void {
    _ = a;
    if (vm.stack.sp < 3) {
        return vm.reporter.reportRuntimeError(null, ErrorCode.VARIABLE_NOT_FOUND, "Stack underflow", .{});
    }

    const value = try vm.stack.pop();
    const index = try vm.stack.pop();
    const array_frame = try vm.stack.pop();

    const index_val = switch (index.value) {
        .int => |i| if (i < 0) {
            return vm.reporter.reportRuntimeError(null, ErrorCode.VARIABLE_NOT_FOUND, "Array index cannot be negative: {}", .{i});
        } else @as(u32, @intCast(i)),
        .byte => |u| @as(u32, u),
        .tetra => |t| @as(u32, t),
        else => {
            return vm.reporter.reportRuntimeError(null, ErrorCode.VARIABLE_NOT_FOUND, "Invalid array index type: {s}", .{@tagName(index.value)});
        },
    };

    switch (array_frame.value) {
        .array => |arr| {
            if (index_val >= arr.capacity) {
                return vm.reporter.reportRuntimeError(null, ErrorCode.VARIABLE_NOT_FOUND, "Array index out of bounds: {}", .{index_val});
            }

            const current_element = if (index_val < arr.elements.len) arr.elements[index_val] else nothing_value;

            const result = switch (current_element) {
                .int => |current| switch (value.value) {
                    .int => |val| HIRValue{ .int = current + val },
                    .byte => |val| HIRValue{ .int = current + val },
                    .float => |val| HIRValue{ .int = current + @as(i64, @intFromFloat(val)) },
                    else => return vm.reporter.reportRuntimeError(null, ErrorCode.VARIABLE_NOT_FOUND, "Cannot add {s} to int", .{@tagName(value.value)}),
                },
                .byte => |current| switch (value.value) {
                    .int => |val| HIRValue{ .byte = @as(u8, @intCast((current + @as(u8, @intCast(val)) & 0xFF))) },
                    .byte => |val| HIRValue{ .byte = @as(u8, @intCast((current + val) & 0xFF)) },
                    else => return vm.reporter.reportRuntimeError(null, ErrorCode.VARIABLE_NOT_FOUND, "Cannot add {s} to byte", .{@tagName(value.value)}),
                },
                .float => |current| switch (value.value) {
                    .int => |val| HIRValue{ .float = current + @as(f64, @floatFromInt(val)) },
                    .byte => |val| HIRValue{ .float = current + @as(f64, @floatFromInt(val)) },
                    .float => |val| HIRValue{ .float = current + val },
                    else => return vm.reporter.reportRuntimeError(null, ErrorCode.VARIABLE_NOT_FOUND, "Cannot add {s} to float", .{@tagName(value.value)}),
                },
                else => return vm.reporter.reportRuntimeError(null, ErrorCode.VARIABLE_NOT_FOUND, "Cannot perform arithmetic on {s}", .{@tagName(current_element)}),
            };

            var mutable_arr = arr;
            if (index_val >= mutable_arr.elements.len) {
                const new_elements = try vm.allocator.realloc(mutable_arr.elements, index_val + 1);
                mutable_arr.elements = new_elements;
                for (mutable_arr.elements[arr.elements.len .. index_val + 1]) |*element| {
                    element.* = nothing_value;
                }
            }
            mutable_arr.elements[index_val] = result;

            try vm.stack.push(HIRFrame.initFromHIRValue(result));
        },
        else => {
            return vm.reporter.reportRuntimeError(null, ErrorCode.VARIABLE_NOT_FOUND, "Cannot perform compound assignment on non-array value: {s}", .{@tagName(array_frame.value)});
        },
    }
}

fn arrayGetAndSub(vm: anytype, a: anytype) !void {
    _ = a;
    if (vm.stack.sp < 3) {
        return vm.reporter.reportRuntimeError(null, ErrorCode.VARIABLE_NOT_FOUND, "Stack underflow", .{});
    }

    const value = try vm.stack.pop();
    const index = try vm.stack.pop();
    const array_frame = try vm.stack.pop();

    const index_val = switch (index.value) {
        .int => |i| if (i < 0) {
            return vm.reporter.reportRuntimeError(null, ErrorCode.VARIABLE_NOT_FOUND, "Array index cannot be negative: {}", .{i});
        } else @as(u32, @intCast(i)),
        .byte => |u| @as(u32, u),
        .tetra => |t| @as(u32, t),
        else => {
            return vm.reporter.reportRuntimeError(null, ErrorCode.VARIABLE_NOT_FOUND, "Invalid array index type: {s}", .{@tagName(index.value)});
        },
    };

    switch (array_frame.value) {
        .array => |arr| {
            if (index_val >= arr.capacity) {
                return vm.reporter.reportRuntimeError(null, ErrorCode.VARIABLE_NOT_FOUND, "Array index out of bounds: {}", .{index_val});
            }

            const current_element = if (index_val < arr.elements.len) arr.elements[index_val] else nothing_value;

            const result = switch (current_element) {
                .int => |current| switch (value.value) {
                    .int => |val| HIRValue{ .int = current - val },
                    .byte => |val| HIRValue{ .int = current - val },
                    .float => |val| HIRValue{ .int = current - @as(i64, @intFromFloat(val)) },
                    else => return vm.reporter.reportRuntimeError(null, ErrorCode.VARIABLE_NOT_FOUND, "Cannot subtract {s} from int", .{@tagName(value.value)}),
                },
                .byte => |current| switch (value.value) {
                    .int => |val| HIRValue{ .byte = @as(u8, @intCast((current - @as(u8, @intCast(val))) & 0xFF)) },
                    .byte => |val| HIRValue{ .byte = @as(u8, @intCast((current - val) & 0xFF)) },
                    else => return vm.reporter.reportRuntimeError(null, ErrorCode.VARIABLE_NOT_FOUND, "Cannot subtract {s} from byte", .{@tagName(value.value)}),
                },
                .float => |current| switch (value.value) {
                    .int => |val| HIRValue{ .float = current - @as(f64, @floatFromInt(val)) },
                    .byte => |val| HIRValue{ .float = current - @as(f64, @floatFromInt(val)) },
                    .float => |val| HIRValue{ .float = current - val },
                    else => return vm.reporter.reportRuntimeError(null, ErrorCode.VARIABLE_NOT_FOUND, "Cannot subtract {s} from float", .{@tagName(value.value)}),
                },
                else => return vm.reporter.reportRuntimeError(null, ErrorCode.VARIABLE_NOT_FOUND, "Cannot perform arithmetic on {s}", .{@tagName(current_element)}),
            };

            var mutable_arr = arr;
            if (index_val >= mutable_arr.elements.len) {
                const new_elements = try vm.allocator.realloc(mutable_arr.elements, index_val + 1);
                mutable_arr.elements = new_elements;
                for (mutable_arr.elements[arr.elements.len .. index_val + 1]) |*element| {
                    element.* = nothing_value;
                }
            }
            mutable_arr.elements[index_val] = result;

            try vm.stack.push(HIRFrame.initFromHIRValue(result));
        },
        else => {
            return vm.reporter.reportRuntimeError(null, ErrorCode.VARIABLE_NOT_FOUND, "Cannot perform compound assignment on non-array value: {s}", .{@tagName(array_frame.value)});
        },
    }
}

fn arrayGetAndMul(vm: anytype, a: anytype) !void {
    _ = a;
    if (vm.stack.sp < 3) {
        return vm.reporter.reportRuntimeError(null, ErrorCode.VARIABLE_NOT_FOUND, "Stack underflow", .{});
    }

    const value = try vm.stack.pop();
    const index = try vm.stack.pop();
    const array_frame = try vm.stack.pop();

    const index_val = switch (index.value) {
        .int => |i| if (i < 0) {
            return vm.reporter.reportRuntimeError(null, ErrorCode.VARIABLE_NOT_FOUND, "Array index cannot be negative: {}", .{i});
        } else @as(u32, @intCast(i)),
        .byte => |u| @as(u32, u),
        .tetra => |t| @as(u32, t),
        else => {
            return vm.reporter.reportRuntimeError(null, ErrorCode.VARIABLE_NOT_FOUND, "Invalid array index type: {s}", .{@tagName(index.value)});
        },
    };

    switch (array_frame.value) {
        .array => |arr| {
            if (index_val >= arr.capacity) {
                return vm.reporter.reportRuntimeError(null, ErrorCode.VARIABLE_NOT_FOUND, "Array index out of bounds: {}", .{index_val});
            }

            const current_element = if (index_val < arr.elements.len) arr.elements[index_val] else nothing_value;

            const result = switch (current_element) {
                .int => |current| switch (value.value) {
                    .int => |val| HIRValue{ .int = current * val },
                    .byte => |val| HIRValue{ .int = current * val },
                    .float => |val| HIRValue{ .int = current * @as(i64, @intFromFloat(val)) },
                    else => return vm.reporter.reportRuntimeError(null, ErrorCode.VARIABLE_NOT_FOUND, "Cannot multiply int by {s}", .{@tagName(value.value)}),
                },
                .byte => |current| switch (value.value) {
                    .int => |val| HIRValue{ .byte = @as(u8, @intCast((current * @as(u8, @intCast(val))) & 0xFF)) },
                    .byte => |val| HIRValue{ .byte = @as(u8, @intCast((current * val) & 0xFF)) },
                    else => return vm.reporter.reportRuntimeError(null, ErrorCode.VARIABLE_NOT_FOUND, "Cannot multiply byte by {s}", .{@tagName(value.value)}),
                },
                .float => |current| switch (value.value) {
                    .int => |val| HIRValue{ .float = current * @as(f64, @floatFromInt(val)) },
                    .byte => |val| HIRValue{ .float = current * @as(f64, @floatFromInt(val)) },
                    .float => |val| HIRValue{ .float = current * val },
                    else => return vm.reporter.reportRuntimeError(null, ErrorCode.VARIABLE_NOT_FOUND, "Cannot multiply float by {s}", .{@tagName(value.value)}),
                },
                else => return vm.reporter.reportRuntimeError(null, ErrorCode.VARIABLE_NOT_FOUND, "Cannot perform arithmetic on {s}", .{@tagName(current_element)}),
            };

            var mutable_arr = arr;
            if (index_val >= mutable_arr.elements.len) {
                const new_elements = try vm.allocator.realloc(mutable_arr.elements, index_val + 1);
                mutable_arr.elements = new_elements;
                for (mutable_arr.elements[arr.elements.len .. index_val + 1]) |*element| {
                    element.* = nothing_value;
                }
            }
            mutable_arr.elements[index_val] = result;

            try vm.stack.push(HIRFrame.initFromHIRValue(result));
        },
        else => {
            return vm.reporter.reportRuntimeError(null, ErrorCode.VARIABLE_NOT_FOUND, "Cannot perform compound assignment on non-array value: {s}", .{@tagName(array_frame.value)});
        },
    }
}

fn arrayGetAndDiv(vm: anytype, a: anytype) !void {
    _ = a;
    if (vm.stack.sp < 3) {
        return vm.reporter.reportRuntimeError(null, ErrorCode.VARIABLE_NOT_FOUND, "Stack underflow", .{});
    }

    const value = try vm.stack.pop();
    const index = try vm.stack.pop();
    const array_frame = try vm.stack.pop();

    const index_val = switch (index.value) {
        .int => |i| if (i < 0) {
            return vm.reporter.reportRuntimeError(null, ErrorCode.VARIABLE_NOT_FOUND, "Array index cannot be negative: {}", .{i});
        } else @as(u32, @intCast(i)),
        .byte => |u| @as(u32, u),
        .tetra => |t| @as(u32, t),
        else => {
            return vm.reporter.reportRuntimeError(null, ErrorCode.VARIABLE_NOT_FOUND, "Invalid array index type: {s}", .{@tagName(index.value)});
        },
    };

    switch (array_frame.value) {
        .array => |arr| {
            if (index_val >= arr.capacity) {
                return vm.reporter.reportRuntimeError(null, ErrorCode.VARIABLE_NOT_FOUND, "Array index out of bounds: {}", .{index_val});
            }

            const current_element = if (index_val < arr.elements.len) arr.elements[index_val] else nothing_value;

            const result = switch (current_element) {
                .int => |current| blk: {
                    const denom = switch (value.value) {
                        .int => |v| v,
                        .byte => |v| @as(i64, v),
                        .float => |v| @as(i64, @intFromFloat(v)),
                        else => return vm.reporter.reportRuntimeError(null, ErrorCode.VARIABLE_NOT_FOUND, "Invalid divisor for int: {s}", .{@tagName(value.value)}),
                    };
                    if (denom == 0) {
                        return vm.reporter.reportRuntimeError(null, ErrorCode.DIVISION_BY_ZERO, "Division by zero", .{});
                    }
                    const res = @as(f64, @floatFromInt(current)) / @as(f64, @floatFromInt(denom));
                    break :blk HIRValue{ .float = res };
                },
                .byte => |current| blk: {
                    const denom: u8 = switch (value.value) {
                        .int => |v| @as(u8, @intCast(if (v < 0) 0 else v)),
                        .byte => |v| v,
                        else => return vm.reporter.reportRuntimeError(null, ErrorCode.VARIABLE_NOT_FOUND, "Invalid divisor for byte: {s}", .{@tagName(value.value)}),
                    };
                    if (denom == 0) {
                        return vm.reporter.reportRuntimeError(null, ErrorCode.DIVISION_BY_ZERO, "Division by zero", .{});
                    }
                    const q = @as(u8, @intCast(current / denom));
                    break :blk HIRValue{ .byte = q };
                },
                .float => |current| blk: {
                    const denom = switch (value.value) {
                        .int => |v| @as(f64, @floatFromInt(v)),
                        .byte => |v| @as(f64, @floatFromInt(v)),
                        .float => |v| v,
                        else => return vm.reporter.reportRuntimeError(null, ErrorCode.VARIABLE_NOT_FOUND, "Invalid divisor for float: {s}", .{@tagName(value.value)}),
                    };
                    if (denom == 0.0) {
                        return vm.reporter.reportRuntimeError(null, ErrorCode.DIVISION_BY_ZERO, "Division by zero", .{});
                    }
                    break :blk HIRValue{ .float = current / denom };
                },
                else => return vm.reporter.reportRuntimeError(null, ErrorCode.VARIABLE_NOT_FOUND, "Cannot perform arithmetic on {s}", .{@tagName(current_element)}),
            };

            var mutable_arr = arr;
            if (index_val >= mutable_arr.elements.len) {
                const new_elements = try vm.allocator.realloc(mutable_arr.elements, index_val + 1);
                mutable_arr.elements = new_elements;
                for (mutable_arr.elements[arr.elements.len .. index_val + 1]) |*element| {
                    element.* = nothing_value;
                }
            }
            mutable_arr.elements[index_val] = result;

            try vm.stack.push(HIRFrame.initFromHIRValue(result));
        },
        else => {
            return vm.reporter.reportRuntimeError(null, ErrorCode.VARIABLE_NOT_FOUND, "Cannot perform compound assignment on non-array value: {s}", .{@tagName(array_frame.value)});
        },
    }
}

fn arrayGetAndMod(vm: anytype, a: anytype) !void {
    _ = a;
    if (vm.stack.sp < 3) {
        return vm.reporter.reportRuntimeError(null, ErrorCode.VARIABLE_NOT_FOUND, "Stack underflow", .{});
    }

    const value = try vm.stack.pop();
    const index = try vm.stack.pop();
    const array_frame = try vm.stack.pop();

    const index_val = switch (index.value) {
        .int => |i| if (i < 0) {
            return vm.reporter.reportRuntimeError(null, ErrorCode.VARIABLE_NOT_FOUND, "Array index cannot be negative: {}", .{i});
        } else @as(u32, @intCast(i)),
        .byte => |u| @as(u32, u),
        .tetra => |t| @as(u32, t),
        else => {
            return vm.reporter.reportRuntimeError(null, ErrorCode.VARIABLE_NOT_FOUND, "Invalid array index type: {s}", .{@tagName(index.value)});
        },
    };

    switch (array_frame.value) {
        .array => |arr| {
            if (index_val >= arr.capacity) {
                return vm.reporter.reportRuntimeError(null, ErrorCode.VARIABLE_NOT_FOUND, "Array index out of bounds: {}", .{index_val});
            }

            const current_element = if (index_val < arr.elements.len) arr.elements[index_val] else nothing_value;

            const result = switch (current_element) {
                .int => |current| blk: {
                    const rhs = switch (value.value) {
                        .int => |v| v,
                        .byte => |v| @as(i64, v),
                        else => return vm.reporter.reportRuntimeError(null, ErrorCode.VARIABLE_NOT_FOUND, "Invalid modulo for int: {s}", .{@tagName(value.value)}),
                    };
                    if (rhs == 0) {
                        return vm.reporter.reportRuntimeError(null, ErrorCode.DIVISION_BY_ZERO, "Modulo by zero", .{});
                    }
                    break :blk HIRValue{ .int = @mod(current, rhs) };
                },
                .byte => |current| blk: {
                    const rhs: u8 = switch (value.value) {
                        .int => |v| @as(u8, @intCast(if (v < 0) 0 else v)),
                        .byte => |v| v,
                        else => return vm.reporter.reportRuntimeError(null, ErrorCode.VARIABLE_NOT_FOUND, "Invalid modulo for byte: {s}", .{@tagName(value.value)}),
                    };
                    if (rhs == 0) {
                        return vm.reporter.reportRuntimeError(null, ErrorCode.DIVISION_BY_ZERO, "Modulo by zero", .{});
                    }
                    break :blk HIRValue{ .byte = @as(u8, @intCast(@mod(current, rhs))) };
                },
                .float => |current| blk: {
                    const rhs = switch (value.value) {
                        .int => |v| @as(f64, @floatFromInt(v)),
                        .byte => |v| @as(f64, @floatFromInt(v)),
                        .float => |v| v,
                        else => return vm.reporter.reportRuntimeError(null, ErrorCode.VARIABLE_NOT_FOUND, "Invalid modulo for float: {s}", .{@tagName(value.value)}),
                    };
                    if (rhs == 0.0) {
                        return vm.reporter.reportRuntimeError(null, ErrorCode.DIVISION_BY_ZERO, "Modulo by zero", .{});
                    }
                    break :blk HIRValue{ .float = @mod(current, rhs) };
                },
                else => return vm.reporter.reportRuntimeError(null, ErrorCode.VARIABLE_NOT_FOUND, "Cannot perform arithmetic on {s}", .{@tagName(current_element)}),
            };

            var mutable_arr = arr;
            if (index_val >= mutable_arr.elements.len) {
                const new_elements = try vm.allocator.realloc(mutable_arr.elements, index_val + 1);
                mutable_arr.elements = new_elements;
                for (mutable_arr.elements[arr.elements.len .. index_val + 1]) |*element| {
                    element.* = nothing_value;
                }
            }
            mutable_arr.elements[index_val] = result;

            try vm.stack.push(HIRFrame.initFromHIRValue(result));
        },
        else => {
            return vm.reporter.reportRuntimeError(null, ErrorCode.VARIABLE_NOT_FOUND, "Cannot perform compound assignment on non-array value: {s}", .{@tagName(array_frame.value)});
        },
    }
}

fn arrayGetAndPow(vm: anytype, a: anytype) !void {
    _ = a;
    if (vm.stack.sp < 3) {
        return vm.reporter.reportRuntimeError(null, ErrorCode.VARIABLE_NOT_FOUND, "Stack underflow", .{});
    }

    const value = try vm.stack.pop();
    const index = try vm.stack.pop();
    const array_frame = try vm.stack.pop();

    const index_val = switch (index.value) {
        .int => |i| if (i < 0) {
            return vm.reporter.reportRuntimeError(null, ErrorCode.VARIABLE_NOT_FOUND, "Array index cannot be negative: {}", .{i});
        } else @as(u32, @intCast(i)),
        .byte => |u| @as(u32, u),
        .tetra => |t| @as(u32, t),
        else => {
            return vm.reporter.reportRuntimeError(null, ErrorCode.VARIABLE_NOT_FOUND, "Invalid array index type: {s}", .{@tagName(index.value)});
        },
    };

    switch (array_frame.value) {
        .array => |arr| {
            if (index_val >= arr.capacity) {
                return vm.reporter.reportRuntimeError(null, ErrorCode.VARIABLE_NOT_FOUND, "Array index out of bounds: {}", .{index_val});
            }

            const current_element = if (index_val < arr.elements.len) arr.elements[index_val] else nothing_value;

            const result = switch (current_element) {
                .int => |current| blk: {
                    const exp_i: i64 = switch (value.value) {
                        .int => |v| v,
                        .byte => |v| @as(i64, v),
                        .float => |v| @as(i64, @intFromFloat(v)),
                        else => return vm.reporter.reportRuntimeError(null, ErrorCode.VARIABLE_NOT_FOUND, "Invalid exponent for int: {s}", .{@tagName(value.value)}),
                    };
                    break :blk HIRValue{ .int = std.math.pow(i64, current, exp_i) };
                },
                .byte => |current| blk: {
                    const exp_b: u8 = switch (value.value) {
                        .int => |v| @as(u8, @intCast(if (v < 0) 0 else v)),
                        .byte => |v| v,
                        else => return vm.reporter.reportRuntimeError(null, ErrorCode.VARIABLE_NOT_FOUND, "Invalid exponent for byte: {s}", .{@tagName(value.value)}),
                    };
                    const p: u32 = std.math.pow(u32, @as(u32, current), @as(u32, exp_b));
                    break :blk HIRValue{ .byte = if (p > 255) 255 else @intCast(p) };
                },
                .float => |current| blk: {
                    const exp_f = switch (value.value) {
                        .int => |v| @as(f64, @floatFromInt(v)),
                        .byte => |v| @as(f64, @floatFromInt(v)),
                        .float => |v| v,
                        else => return vm.reporter.reportRuntimeError(null, ErrorCode.VARIABLE_NOT_FOUND, "Invalid exponent for float: {s}", .{@tagName(value.value)}),
                    };
                    break :blk HIRValue{ .float = std.math.pow(f64, current, exp_f) };
                },
                else => return vm.reporter.reportRuntimeError(null, ErrorCode.VARIABLE_NOT_FOUND, "Cannot perform arithmetic on {s}", .{@tagName(current_element)}),
            };

            var mutable_arr = arr;
            if (index_val >= mutable_arr.elements.len) {
                const new_elements = try vm.allocator.realloc(mutable_arr.elements, index_val + 1);
                mutable_arr.elements = new_elements;
                for (mutable_arr.elements[arr.elements.len .. index_val + 1]) |*element| {
                    element.* = nothing_value;
                }
            }
            mutable_arr.elements[index_val] = result;

            try vm.stack.push(HIRFrame.initFromHIRValue(result));
        },
        else => {
            return vm.reporter.reportRuntimeError(null, ErrorCode.VARIABLE_NOT_FOUND, "Cannot perform compound assignment on non-array value: {s}", .{@tagName(array_frame.value)});
        },
    }
}
