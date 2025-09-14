const std = @import("std");
const HIRValue = @import("../../codegen/hir/soxa_values.zig").HIRValue;
const HIRStructField = @import("../../codegen/hir/soxa_values.zig").HIRStructField;
const HIRMapEntry = @import("../../codegen/hir/soxa_values.zig").HIRMapEntry;
const HIRType = @import("../../codegen/hir/soxa_types.zig").HIRType;
const Core = @import("../core.zig");
const HIRFrame = Core.HIRFrame;
const Errors = @import("../../utils/errors.zig");
const ErrorList = Errors.ErrorList;
const ErrorCode = Errors.ErrorCode;
const debug_print = @import("../calls/print.zig");

// Execute array operations. Accepts the VM as `anytype` to avoid import cycles.
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
    } else if (@hasField(@TypeOf(instruction), "ArrayLen")) {
        try arrayLen(vm);
    } else if (@hasField(@TypeOf(instruction), "ArrayConcat")) {
        try arrayConcat(vm);
    } else if (@hasField(@TypeOf(instruction), "Range")) {
        try arrayRange(vm, instruction.Range);
    } else {
        unreachable;
    }
}

/// Create new array with specified size
fn arrayNew(vm: anytype, a: anytype) !void {
    // CRITICAL FIX: For empty arrays (size=0), allocate minimum capacity for growth
    const initial_capacity = if (a.size == 0) 8 else a.size; // Start with capacity 8 for empty arrays
    const elements = try vm.allocator.alloc(HIRValue, initial_capacity);

    // Initialize elements: first 'size' with type default, remainder with 'nothing'
    const default_value = getDefaultValue(a.element_type);
    var i: usize = 0;
    while (i < @as(usize, @intCast(a.size)) and i < elements.len) : (i += 1) {
        elements[i] = default_value;
    }
    while (i < elements.len) : (i += 1) {
        elements[i] = HIRValue.nothing;
    }

    const new_array = HIRValue{
        .array = .{
            .elements = elements,
            .capacity = @intCast(initial_capacity),
            .element_type = a.element_type,
        },
    };

    try vm.stack.push(HIRFrame.initFromHIRValue(new_array));
}

/// Get array element by index: array[index]
fn arrayGet(vm: anytype, a: anytype) !void {
    const index = try vm.stack.pop(); // Index
    const array = try vm.stack.pop(); // Array

    // IMPROVED: Handle different index types more gracefully
    const index_val = switch (index.value) {
        .int => |i| if (i < 0) {
            return vm.reporter.reportRuntimeError(null, ErrorCode.VARIABLE_NOT_FOUND, "Array index cannot be negative: {}", .{i});
        } else @as(u32, @intCast(i)),
        .byte => |u| @as(u32, u),
        .tetra => |t| @as(u32, t), // Allow tetra values as indices
        .string => |s| blk: {
            // GRACEFUL: Try to parse string as integer
            const parsed = std.fmt.parseInt(i64, s, 10) catch {
                try vm.stack.push(array); // Push original array back
                return;
            };
            if (parsed < 0) {
                return vm.reporter.reportRuntimeError(null, ErrorCode.VARIABLE_NOT_FOUND, "Array index cannot be negative: {}", .{parsed});
            }
            break :blk @as(u32, @intCast(parsed));
        },
        .nothing => {
            try vm.stack.push(array); // Push original array back
            return;
        },
        else => {
            try vm.stack.push(array); // Push original array back
            return;
        },
    };

    switch (array.value) {
        .array => |arr| {
            // Calculate actual length (stop at first nothing element)
            var actual_length: u32 = 0;
            for (arr.elements) |elem| {
                if (std.meta.eql(elem, HIRValue.nothing)) break;
                actual_length += 1;
            }

            if (a.bounds_check and index_val >= actual_length) {
                return ErrorList.IndexOutOfBounds;
            }

            // Get the element (or nothing if out of bounds)
            const element = if (index_val < actual_length) arr.elements[index_val] else HIRValue.nothing;
            try vm.stack.push(HIRFrame.initFromHIRValue(element));
        },
        .string => |s| {
            // GRACEFUL: Allow string indexing (return character as string)
            if (index_val >= s.len) {
                try vm.stack.push(HIRFrame.initFromHIRValue(HIRValue.nothing));
                return;
            }
            const char_str = s[index_val .. index_val + 1];
            try vm.stack.push(HIRFrame.initFromHIRValue(HIRValue{ .string = char_str }));
        },
        .nothing => {
            // GRACEFUL: Treat 'nothing' as empty array - any index returns nothing
            try vm.stack.push(HIRFrame.initFromHIRValue(HIRValue.nothing));
        },
        else => {
            return vm.reporter.reportRuntimeError(null, ErrorCode.VARIABLE_NOT_FOUND, "Cannot index non-array value: {s}", .{@tagName(array.value)});
        },
    }
}

/// Set array element by index
fn arraySet(vm: anytype, a: anytype) !void {
    // Stack order (top to bottom): value, index, array
    const value = try vm.stack.pop(); // Value to set
    const index = try vm.stack.pop(); // Index
    const array_frame = try vm.stack.pop(); // Array

    // IMPROVED: Handle different index types more gracefully
    const index_val = switch (index.value) {
        .int => |i| if (i < 0) {
            return vm.reporter.reportRuntimeError(null, ErrorCode.VARIABLE_NOT_FOUND, "Array index cannot be negative: {}", .{i});
        } else @as(u32, @intCast(i)),
        .byte => |u| @as(u32, u),
        .tetra => |t| @as(u32, t), // Allow tetra values as indices
        .string => |s| blk: {
            // GRACEFUL: Try to parse string as integer
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
            try vm.stack.push(array_frame); // Push original array back
            return;
        },
        else => {
            try vm.stack.push(array_frame); // Push original array back
            return;
        },
    };

    switch (array_frame.value) {
        .array => |arr| {
            // Calculate actual length (stop at first nothing element)
            var actual_length: u32 = 0;
            for (arr.elements) |elem| {
                if (std.meta.eql(elem, HIRValue.nothing)) break;
                actual_length += 1;
            }

            if (a.bounds_check and index_val >= actual_length) {
                return ErrorList.IndexOutOfBounds;
            }

            // Create a mutable copy of the array
            var mutable_arr = arr;

            // Ensure we have enough capacity
            if (index_val >= mutable_arr.capacity) {
                const new_capacity = @max(mutable_arr.capacity * 2, index_val + 1);
                const new_elements = try vm.allocator.realloc(mutable_arr.elements, new_capacity);
                mutable_arr.elements = new_elements;
                mutable_arr.capacity = new_capacity;

                // Initialize new elements with nothing
                for (mutable_arr.elements[arr.capacity..new_capacity]) |*element| {
                    element.* = HIRValue.nothing;
                }
            }

            // Set the element
            mutable_arr.elements[index_val] = value.value;

            // Update the array in the frame
            const updated_array = HIRValue{ .array = mutable_arr };
            try vm.stack.push(HIRFrame.initFromHIRValue(updated_array));
        },
        else => {
            return vm.reporter.reportRuntimeError(null, ErrorCode.VARIABLE_NOT_FOUND, "Cannot set element in non-array value: {s}", .{@tagName(array_frame.value)});
        },
    }
}

/// Push element to end of array
fn arrayPush(vm: anytype) !void {
    const value = try vm.stack.pop();
    if (vm.reporter.debug_mode) {
        const t = debug_print.getTypeString(vm, value.value);
        vm.reporter.report(.Debug, .Hint, null, null, "ArrayPush elem_type='{s}'", .{t});
    }
    try handleArrayPush(vm, value.value);
}

/// Pop element from end of array
fn arrayPop(vm: anytype) !void {
    // Pop element from end of array
    const array = try vm.stack.pop(); // Array
    if (vm.reporter.debug_mode) {
        const t = debug_print.getTypeString(vm, array.value);
        vm.reporter.report(.Debug, .Hint, null, null, "ArrayPop array_type='{s}'", .{t});
    }

    switch (array.value) {
        .array => |arr| {
            // Create a mutable copy of the array
            var mutable_arr = arr;

            // Find the last non-nothing element
            var last_index: ?u32 = null;
            for (arr.elements, 0..) |elem, i| {
                if (std.meta.eql(elem, HIRValue.nothing)) break;
                last_index = @intCast(i);
            }

            if (last_index) |idx| {
                // Get the last element
                const last_element = arr.elements[idx];

                // Set that position to nothing
                mutable_arr.elements[idx] = HIRValue.nothing;

                // Push the updated array back first (like original VM)
                const updated_array = HIRValue{ .array = mutable_arr };
                try vm.stack.push(HIRFrame.initFromHIRValue(updated_array));

                // Then push the popped element
                try vm.stack.push(HIRFrame.initFromHIRValue(last_element));
            } else {
                // Array is empty, return error like original VM
                return vm.reporter.reportRuntimeError(null, ErrorCode.VARIABLE_NOT_FOUND, "Cannot pop from empty array", .{});
            }
        },
        .string => |s_val| {
            // GRACEFUL: Handle string pop (delegate to string operations)
            if (s_val.len == 0) {
                return vm.reporter.reportRuntimeError(null, ErrorCode.VARIABLE_NOT_FOUND, "Cannot pop from empty string", .{});
            }

            // Find the start of the last UTF-8 character
            var last_char_start: usize = s_val.len;
            var i: usize = s_val.len;
            while (i > 0) {
                i -= 1;
                const byte = s_val[i];
                // Check if this is the start of a UTF-8 sequence
                if ((byte & 0x80) == 0) {
                    // ASCII character
                    last_char_start = i;
                    break;
                } else if ((byte & 0xC0) == 0xC0) {
                    // Start of a multi-byte sequence
                    last_char_start = i;
                    break;
                }
                // Otherwise, this is a continuation byte, keep going
            }

            const last_char = s_val[last_char_start..s_val.len];
            const remaining = s_val[0..last_char_start];

            // Push updated string first, then popped char to mirror ArrayPop order
            try vm.stack.push(HIRFrame.initString(remaining));
            try vm.stack.push(HIRFrame.initString(last_char));
        },
        else => {
            return vm.reporter.reportRuntimeError(null, ErrorCode.VARIABLE_NOT_FOUND, "Cannot pop from non-array value: {s}", .{@tagName(array.value)});
        },
    }
}

/// Get length of array
fn arrayLen(vm: anytype) !void {
    // Get length of array or string and push as int
    const value = try vm.stack.pop();
    switch (value.value) {
        .array => |arr| {
            var length: u32 = 0;
            for (arr.elements) |elem| {
                if (std.meta.eql(elem, HIRValue.nothing)) break;
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

/// Concatenate two arrays
fn arrayConcat(vm: anytype) !void {
    // Concatenate two arrays
    const b = try vm.stack.pop(); // Second array
    const a = try vm.stack.pop(); // First array

    switch (a.value) {
        .array => |arr_a| {
            switch (b.value) {
                .array => |arr_b| {
                    // Calculate lengths
                    var len_a: u32 = 0;
                    for (arr_a.elements) |elem| {
                        if (std.meta.eql(elem, HIRValue.nothing)) break;
                        len_a += 1;
                    }

                    var len_b: u32 = 0;
                    for (arr_b.elements) |elem| {
                        if (std.meta.eql(elem, HIRValue.nothing)) break;
                        len_b += 1;
                    }

                    // Create new array with combined elements
                    const new_elements = try vm.allocator.alloc(HIRValue, len_a + len_b);

                    // Copy elements from first array
                    for (0..len_a) |i| {
                        new_elements[i] = arr_a.elements[i];
                    }

                    // Copy elements from second array
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

/// Helper function to handle array push operations
fn handleArrayPush(vm: anytype, element_value: HIRValue) !void {
    const array_frame = try vm.stack.pop();
    switch (array_frame.value) {
        .array => |arr| {
            // Work with a mutable copy
            var mutable_arr = arr;

            // If array has no concrete element type yet, infer from first pushed element
            if (mutable_arr.element_type == .Unknown or mutable_arr.element_type == .Nothing) {
                mutable_arr.element_type = switch (element_value) {
                    .int => .Int,
                    .byte => .Byte,
                    .float => .Float,
                    .string => .String,
                    .tetra => .Tetra,
                    .array => .Array,
                    .struct_instance => .Struct,
                    else => mutable_arr.element_type,
                };
            }

            // Find the first nothing element (end of array)
            var insert_index: u32 = 0;
            for (mutable_arr.elements, 0..) |elem, i| {
                if (std.meta.eql(elem, HIRValue.nothing)) {
                    insert_index = @intCast(i);
                    break;
                }
                insert_index = @intCast(i + 1);
            }

            // If we need more capacity, resize
            if (insert_index >= mutable_arr.capacity) {
                const new_capacity = @max(mutable_arr.capacity * 2, insert_index + 1);
                const new_elements = try vm.allocator.realloc(mutable_arr.elements, new_capacity);
                mutable_arr.elements = new_elements;
                mutable_arr.capacity = new_capacity;

                // Initialize new elements with nothing
                for (mutable_arr.elements[arr.capacity..new_capacity]) |*element| {
                    element.* = HIRValue.nothing;
                }
            }

            // Insert the element
            mutable_arr.elements[insert_index] = element_value;

            // Push the modified array back (code generation will handle storing to variable and returning nothing)
            const updated_array = HIRValue{ .array = mutable_arr };
            try vm.stack.push(HIRFrame.initFromHIRValue(updated_array));
        },
        else => {
            return vm.reporter.reportRuntimeError(null, ErrorCode.VARIABLE_NOT_FOUND, "Cannot push to non-array value: {s}", .{@tagName(array_frame.value)});
        },
    }
}

/// Create array from range (start to end inclusive)
fn arrayRange(vm: anytype, r: anytype) !void {
    const end_frame = try vm.stack.pop(); // End value
    const start_frame = try vm.stack.pop(); // Start value

    // Extract integer values from start and end
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

    // Calculate array size
    const size = if (end_val >= start_val)
        @as(u32, @intCast(end_val - start_val + 1))
    else
        0;

    // Allocate array
    const elements = try vm.allocator.alloc(HIRValue, size);

    // Fill array with range values
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

/// Get default value for a given type
fn getDefaultValue(hir_type: HIRType) HIRValue {
    return switch (hir_type) {
        .Int => HIRValue{ .int = 0 },
        .Byte => HIRValue{ .byte = 0 },
        .Float => HIRValue{ .float = 0.0 },
        .String => HIRValue{ .string = "" },
        .Tetra => HIRValue{ .tetra = 0 },
        .Nothing => HIRValue.nothing,
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
        .Function => HIRValue.nothing,
        .Union => HIRValue.nothing,
        .Unknown => HIRValue.nothing,
    };
}
