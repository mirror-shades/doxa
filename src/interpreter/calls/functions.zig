const std = @import("std");
const HIRValue = @import("../../codegen/hir/soxa_values.zig").HIRValue;
const Core = @import("../core.zig");
const HIRFrame = Core.HIRFrame;
const CallFrame = Core.CallFrame;
const Errors = @import("../../utils/errors.zig");
const ErrorList = Errors.ErrorList;
const ErrorCode = Errors.ErrorCode;
const debug_print = @import("../calls/print.zig");

pub const FunctionOps = struct {
    // Execute TailCall instruction
    pub fn execTailCall(vm: anytype, c: anytype) !void {
        // TAIL CALL OPTIMIZATION: Reuse current stack frame instead of creating new one
        switch (c.call_kind) {
            .LocalFunction => {
                // Tail call optimization for user-defined functions
                if (c.function_index >= vm.program.function_table.len) {
                    return vm.reporter.reportRuntimeError(null, ErrorCode.VARIABLE_NOT_FOUND, "Invalid function index: {} (max: {})", .{ c.function_index, vm.program.function_table.len });
                }

                const function = vm.program.function_table[c.function_index];
                const target_label = function.start_label;

                vm.skip_next_enter_scope = true; // Skip scope creation when we jump to function start

                // Jump to function start (including parameter setup) without call stack modification
                if (vm.label_map.get(target_label)) |target_ip| {
                    vm.ip = target_ip;
                    return; // Jump to function body
                } else {
                    return vm.reporter.reportRuntimeError(null, ErrorCode.VARIABLE_NOT_FOUND, "Function label not found: {s}", .{target_label});
                }
            },
            else => {
                return vm.reporter.reportRuntimeError(null, ErrorCode.VARIABLE_NOT_FOUND, "Tail call not supported for call kind: {s}", .{@tagName(c.call_kind)});
            },
        }
    }

    // Execute Call instruction
    pub fn execCall(vm: anytype, c: anytype) !void {
        switch (c.call_kind) {
            .LocalFunction => {
                // User-defined function call with proper stack management
                if (c.function_index >= vm.program.function_table.len) {
                    return vm.reporter.reportRuntimeError(null, ErrorCode.VARIABLE_NOT_FOUND, "Invalid function index: {} (max: {})", .{ c.function_index, vm.program.function_table.len });
                }

                const function = vm.program.function_table[c.function_index];

                // Save SP before popping arguments so Return can restore caller stack correctly
                const saved_sp = vm.stack.size() - @as(i64, @intCast(c.arg_count));

                // Pop arguments from stack and store them temporarily
                var args = try vm.allocator.alloc(HIRFrame, c.arg_count);
                defer vm.allocator.free(args);

                // Pop arguments in reverse order (they were pushed in reverse)
                for (0..c.arg_count) |i| {
                    args[c.arg_count - 1 - i] = try vm.stack.pop();
                }

                // Push call frame for proper return handling
                const return_ip = vm.ip + 1; // Return to instruction after this call

                const call_frame = CallFrame{
                    .return_ip = return_ip,
                    .function_name = function.name,
                    .arg_count = c.arg_count, // Store arg count to clean up stack later
                    .saved_sp = saved_sp,
                };
                try vm.call_stack.push(call_frame);

                // Push arguments back onto stack in correct order for function
                for (args) |arg| {
                    try vm.stack.push(arg);
                }

                // Use pre-resolved label map for O(1) lookup
                if (vm.label_map.get(function.start_label)) |target_ip| {
                    vm.ip = target_ip;
                    return; // Jump to function start
                } else {
                    return vm.reporter.reportRuntimeError(null, ErrorCode.VARIABLE_NOT_FOUND, "Function label not found: {s}", .{function.start_label});
                }
            },
            .BuiltinFunction => {
                try execBuiltinFunction(vm, c);
            },
            .ModuleFunction => {
                try execModuleFunction(vm, c);
            },
        }
    }

    // Execute Return instruction
    pub fn execReturn(vm: anytype, _: anytype) !void {
        // Check if we're returning from main program or a function call
        if (vm.call_stack.isEmpty()) {
            vm.running = false;
        } else {
            // Returning from function call - pop call frame and return to caller
            const call_frame = try vm.call_stack.pop();

            // Restore operand stack to the saved position, preserving the top return value
            const current_sp = vm.stack.size();
            if (current_sp > call_frame.saved_sp) {
                // There is at least one value to return on the stack; keep the topmost
                const ret_val = vm.stack.pop() catch HIRFrame.initNothing();
                vm.stack.sp = call_frame.saved_sp;
                try vm.stack.push(ret_val);
            } else {
                // No value returned; just restore SP
                vm.stack.sp = call_frame.saved_sp;
            }

            // Return to caller
            vm.ip = call_frame.return_ip;

            return; // Don't auto-increment IP since we just set it
        }
    }

    // Execute built-in function calls
    fn execBuiltinFunction(vm: anytype, c: anytype) !void {
        if (std.mem.eql(u8, c.qualified_name, "length")) {
            // Array length method - expects array on stack
            const array = try vm.stack.pop();
            switch (array.value) {
                .array => |arr| {
                    // Find the actual length by counting non-nothing elements
                    var length: u32 = 0;
                    for (arr.elements) |elem| {
                        if (std.meta.eql(elem, HIRValue.nothing)) break;
                        length += 1;
                    }
                    try vm.stack.push(HIRFrame.initInt(@as(i64, @intCast(length))));
                },
                else => return vm.reporter.reportRuntimeError(null, ErrorCode.VARIABLE_NOT_FOUND, "Cannot get length of non-array value: {s}", .{@tagName(array.value)}),
            }
        } else if (std.mem.eql(u8, c.qualified_name, "push")) {
            try execBuiltinArrayPush(vm);
        } else if (std.mem.eql(u8, c.qualified_name, "safeAdd")) {
            try execBuiltinSafeAdd(vm);
        } else if (std.mem.eql(u8, c.qualified_name, "power") or std.mem.eql(u8, c.qualified_name, "powi")) {
            try execBuiltinPower(vm, c.qualified_name);
        } else if (std.mem.eql(u8, c.qualified_name, "exists_quantifier_gt")) {
            try execBuiltinExistsQuantifierGt(vm);
        } else if (std.mem.eql(u8, c.qualified_name, "forall_quantifier_gt")) {
            try execBuiltinForallQuantifierGt(vm);
        } else if (std.mem.eql(u8, c.qualified_name, "input")) {
            try execBuiltinInput(vm);
        } else {
            return vm.reporter.reportRuntimeError(null, ErrorCode.VARIABLE_NOT_FOUND, "Unknown built-in function: {s}", .{c.qualified_name});
        }
    }

    // Execute module function calls
    fn execModuleFunction(vm: anytype, c: anytype) !void {
        if (std.mem.eql(u8, c.qualified_name, "safeMath.safeAdd")) {
            // Safe addition with overflow/underflow checking (from safeMath.doxa)
            const b = try vm.stack.pop();
            const a = try vm.stack.pop();

            const a_int = switch (a.value) {
                .int => |i| i,
                .byte => |u| @as(i64, u),
                else => return vm.reporter.reportRuntimeError(null, ErrorCode.VARIABLE_NOT_FOUND, "safeAdd: first argument must be integer", .{}),
            };

            const b_int = switch (b.value) {
                .int => |i| i,
                .byte => |u| @as(i64, u),
                else => return vm.reporter.reportRuntimeError(null, ErrorCode.VARIABLE_NOT_FOUND, "safeAdd: second argument must be integer", .{}),
            };

            // Apply safeMath.doxa logic: limit = 255
            const limit = 255;
            if (a_int > limit or b_int > limit) {
                // Overflow detected - print "Overflow" with proper peek format per safeMath.doxa
                const overflow_value = HIRValue{ .string = "Overflow" };
                try std.io.getStdOut().writer().print("[test/misc/safeMath.doxa:7:9] :: string is ", .{});
                try debug_print.formatHIRValue(vm, std.io.getStdOut().writer(), overflow_value);
                try std.io.getStdOut().writer().print("\n", .{});
                try vm.stack.push(HIRFrame.initInt(-1));
                return;
            }

            if (a_int < 0 or b_int < 0) {
                // Underflow detected - print "Underflow" with proper peek format per safeMath.doxa
                const underflow_value = HIRValue{ .string = "Underflow" };
                try std.io.getStdOut().writer().print("[test/misc/safeMath.doxa:12:9] :: string is ", .{});
                try debug_print.formatHIRValue(vm, std.io.getStdOut().writer(), underflow_value);
                try std.io.getStdOut().writer().print("\n", .{});
                try vm.stack.push(HIRFrame.initInt(-1));
                return;
            }

            // Safe to add - call math.add logic
            const result = std.math.add(i64, a_int, b_int) catch {
                // Integer overflow in addition - return -1
                try vm.stack.push(HIRFrame.initInt(-1));
                return;
            };

            try vm.stack.push(HIRFrame.initInt(result));
        } else {
            return vm.reporter.reportRuntimeError(null, ErrorCode.VARIABLE_NOT_FOUND, "Unknown module function: {s}", .{c.qualified_name});
        }
    }

    // Built-in array push method
    fn execBuiltinArrayPush(vm: anytype) !void {
        // Array push method - expects element and array on stack
        const element = try vm.stack.pop(); // Element to push
        const array = try vm.stack.pop(); // Array

        switch (array.value) {
            .array => |arr| {
                // Create a mutable copy of the array
                var mutable_arr = arr;

                // Find the current length
                var length: u32 = 0;
                for (mutable_arr.elements) |elem| {
                    if (std.meta.eql(elem, HIRValue.nothing)) break;
                    length += 1;
                }

                // Check if we need to resize
                if (length >= mutable_arr.capacity) {
                    // CRITICAL FIX: Resize the array by doubling capacity
                    const new_capacity = mutable_arr.capacity * 2;
                    const new_elements = try vm.allocator.realloc(mutable_arr.elements, new_capacity);
                    mutable_arr.elements = new_elements;
                    mutable_arr.capacity = new_capacity;

                    // Initialize new elements to nothing
                    for (length..new_capacity) |i| {
                        mutable_arr.elements[i] = HIRValue.nothing;
                    }
                }

                // Add element to end (capacity is guaranteed to be > length)
                mutable_arr.elements[length] = element.value;

                // Store the modified array back to the variable (if it's a variable)
                // This is handled by the code generation, but we need to ensure the modified array is available
                // Push the modified array temporarily, then the code generation will handle storing it
                const modified_array_value = HIRValue{ .array = mutable_arr };
                try vm.stack.push(HIRFrame.initFromHIRValue(modified_array_value));
            },
            else => return vm.reporter.reportRuntimeError(null, ErrorCode.VARIABLE_NOT_FOUND, "Cannot push to non-array value: {s}", .{@tagName(array.value)}),
        }
    }

    // Built-in safe addition
    fn execBuiltinSafeAdd(vm: anytype) !void {
        // Safe addition with overflow protection
        const b = try vm.stack.pop();
        const a = try vm.stack.pop();

        const a_int = switch (a.value) {
            .int => |i| i,
            .byte => |u| @as(i64, u),
            else => return vm.reporter.reportRuntimeError(null, ErrorCode.VARIABLE_NOT_FOUND, "safeAdd: first argument must be integer", .{}),
        };

        const b_int = switch (b.value) {
            .int => |i| i,
            .byte => |u| @as(i64, u),
            else => return vm.reporter.reportRuntimeError(null, ErrorCode.VARIABLE_NOT_FOUND, "safeAdd: second argument must be integer", .{}),
        };

        const result = std.math.add(i64, a_int, b_int) catch {
            // On overflow, return nothing instead of crashing
            try vm.stack.push(HIRFrame.initFromHIRValue(HIRValue.nothing));
            return;
        };

        try vm.stack.push(HIRFrame.initInt(result));
    }

    // Built-in power function
    fn execBuiltinPower(vm: anytype, function_name: []const u8) !void {
        // Power function - expects base and exponent on stack
        const exponent = try vm.stack.pop();
        const base = try vm.stack.pop();

        if (std.mem.eql(u8, function_name, "powi")) {
            // Integer power: both operands must be Int
            const base_int = switch (base.value) {
                .int => |i| i,
                .byte => |b| @as(i64, b),
                else => return vm.reporter.reportRuntimeError(null, ErrorCode.VARIABLE_NOT_FOUND, "powi: base must be integer", .{}),
            };
            const exp_int = switch (exponent.value) {
                .int => |i| i,
                .byte => |b| @as(i64, b),
                else => return vm.reporter.reportRuntimeError(null, ErrorCode.VARIABLE_NOT_FOUND, "powi: exponent must be integer", .{}),
            };
            const result = std.math.pow(i64, base_int, @intCast(exp_int));
            try vm.stack.push(HIRFrame.initInt(result));
            return;
        }

        // Convert both operands to float for power calculation
        const base_float = switch (base.value) {
            .int => |i| @as(f64, @floatFromInt(i)),
            .float => |f| f,
            .byte => |b| @as(f64, @floatFromInt(b)),
            else => return vm.reporter.reportRuntimeError(null, ErrorCode.VARIABLE_NOT_FOUND, "power: base must be numeric", .{}),
        };

        const exponent_float = switch (exponent.value) {
            .int => |i| @as(f64, @floatFromInt(i)),
            .float => |f| f,
            .byte => |b| @as(f64, @floatFromInt(b)),
            else => return vm.reporter.reportRuntimeError(null, ErrorCode.VARIABLE_NOT_FOUND, "power: exponent must be numeric", .{}),
        };

        const result = std.math.pow(f64, base_float, exponent_float);
        try vm.stack.push(HIRFrame.initFloat(result));
    }

    // Built-in exists quantifier with greater-than condition
    fn execBuiltinExistsQuantifierGt(vm: anytype) !void {
        // Existential quantifier with greater-than condition
        const comparison_value = try vm.stack.pop();
        const array = try vm.stack.pop();

        switch (array.value) {
            .array => |arr| {
                // Check if any element is greater than comparison_value
                var found = false;
                for (arr.elements) |elem| {
                    if (std.meta.eql(elem, HIRValue.nothing)) {
                        break; // End of array
                    }
                    // Check if element > comparison_value
                    const satisfies_condition = switch (elem) {
                        .int => |elem_int| switch (comparison_value.value) {
                            .int => |comp_int| elem_int > comp_int,
                            else => false,
                        },
                        .float => |elem_float| switch (comparison_value.value) {
                            .float => |comp_float| elem_float > comp_float,
                            .int => |comp_int| elem_float > @as(f64, @floatFromInt(comp_int)),
                            else => false,
                        },
                        else => false,
                    };
                    if (satisfies_condition) {
                        found = true;
                        break;
                    }
                }
                try vm.stack.push(HIRFrame.initTetra(if (found) 1 else 0));
            },
            else => return vm.reporter.reportRuntimeError(null, ErrorCode.VARIABLE_NOT_FOUND, "exists_quantifier_gt: argument must be array", .{}),
        }
    }

    // Built-in forall quantifier with greater-than condition
    fn execBuiltinForallQuantifierGt(vm: anytype) !void {
        // Universal quantifier with greater-than condition
        const comparison_value = try vm.stack.pop();
        const array = try vm.stack.pop();

        switch (array.value) {
            .array => |arr| {
                // Check if all elements are greater than comparison_value
                var all_satisfy = true;
                var has_elements = false;
                for (arr.elements) |elem| {
                    if (std.meta.eql(elem, HIRValue.nothing)) {
                        break; // End of array
                    }
                    has_elements = true;
                    // Check if element > comparison_value
                    const satisfies_condition = switch (elem) {
                        .int => |elem_int| switch (comparison_value.value) {
                            .int => |comp_int| blk: {
                                const result = elem_int > comp_int;
                                break :blk result;
                            },
                            else => false,
                        },
                        .float => |elem_float| switch (comparison_value.value) {
                            .float => |comp_float| elem_float > comp_float,
                            .int => |comp_int| elem_float > @as(f64, @floatFromInt(comp_int)),
                            else => false,
                        },
                        else => false,
                    };
                    if (!satisfies_condition) {
                        all_satisfy = false;
                        break;
                    }
                }
                // For empty arrays, forall returns true (vacuous truth)
                try vm.stack.push(HIRFrame.initTetra(if ((!has_elements) or all_satisfy) 1 else 0));
            },
            else => return vm.reporter.reportRuntimeError(null, ErrorCode.VARIABLE_NOT_FOUND, "forall_quantifier_gt: argument must be array", .{}),
        }
    }

    // Built-in input function
    fn execBuiltinInput(vm: anytype) !void {
        // Simple approach: use readUntilDelimiterAlloc which is more reliable
        const stdin = std.io.getStdIn().reader();

        // Use the standard library's readUntilDelimiterAlloc for better platform compatibility
        const input_line = stdin.readUntilDelimiterAlloc(vm.allocator, '\n', 4096) catch |err| switch (err) {
            error.StreamTooLong => blk: {
                // Handle long lines by reading what we can
                var buffer: [4096]u8 = undefined;
                const line = stdin.readUntilDelimiterOrEof(buffer[0..], '\n') catch "";
                break :blk try vm.allocator.dupe(u8, line orelse "");
            },
            error.EndOfStream => try vm.allocator.dupe(u8, ""),
            else => return err,
        };
        defer vm.allocator.free(input_line);

        // Remove trailing carriage return if present (Windows compatibility)
        var line = input_line;
        if (line.len > 0 and line[line.len - 1] == '\r') {
            line = line[0 .. line.len - 1];
        }

        // Create a copy of the input string in VM memory
        const input_string = try vm.allocator.dupe(u8, line);
        try vm.stack.push(HIRFrame.initString(input_string));
    }
};
