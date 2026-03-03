const std = @import("std");
const HIRValue = @import("../../codegen/hir/soxa_values.zig").HIRValue;
const Core = @import("../core.zig");
const HIRFrame = Core.HIRFrame;
const CallFrame = Core.CallFrame;
const Errors = @import("../../utils/errors.zig");
const ErrorCode = Errors.ErrorCode;

pub const FunctionOps = struct {
    pub fn execTailCall(vm: anytype, c: anytype) !void {
        // TAIL CALL OPTIMIZATION: Reuse current stack frame instead of creating new one
        switch (c.call_kind) {
            .LocalFunction => {
                if (c.function_index >= vm.program.function_table.len) {
                    return vm.reporter.reportRuntimeError(null, ErrorCode.VARIABLE_NOT_FOUND, "Invalid function index: {} (max: {})", .{ c.function_index, vm.program.function_table.len });
                }

                const function = vm.program.function_table[c.function_index];
                vm.skip_next_enter_scope = true;

                if (function.start_ip != 0) {
                    vm.ip = function.start_ip;
                    return;
                }
                return vm.reporter.reportRuntimeError(null, ErrorCode.VARIABLE_NOT_FOUND, "Function label not resolved: {s}", .{function.start_label});
            },
            else => {
                return vm.reporter.reportRuntimeError(null, ErrorCode.VARIABLE_NOT_FOUND, "Tail call not supported for call kind: {s}", .{@tagName(c.call_kind)});
            },
        }
    }

    pub fn execCall(vm: anytype, c: anytype) !void {
        switch (c.call_kind) {
            .LocalFunction => {
                if (c.function_index >= vm.program.function_table.len) {
                    return vm.reporter.reportRuntimeError(null, ErrorCode.VARIABLE_NOT_FOUND, "Invalid function index: {} (max: {})", .{ c.function_index, vm.program.function_table.len });
                }

                const function = vm.program.function_table[c.function_index];

                const saved_sp = vm.stack.size() - @as(i64, @intCast(c.arg_count));

                const return_ip = vm.ip + 1;

                const call_frame = CallFrame{
                    .return_ip = return_ip,
                    .function_name = function.name,
                    .arg_count = c.arg_count,
                    .saved_sp = saved_sp,
                };
                try vm.call_stack.push(call_frame);

                if (function.start_ip != 0) {
                    vm.ip = function.start_ip;
                    return;
                }
                return vm.reporter.reportRuntimeError(null, ErrorCode.VARIABLE_NOT_FOUND, "Function label not resolved: {s}", .{function.start_label});
            },
            .BuiltinFunction => {
                try execBuiltinFunction(vm, c);
            },
            .ModuleFunction => {
                try execModuleFunction(vm, c);
            },
        }
    }

    pub fn execReturn(vm: anytype, payload: anytype) !void {
        if (vm.call_stack.isEmpty()) {
            vm.running = false;
        } else {
            const call_frame = try vm.call_stack.pop();

            const current_sp = vm.stack.size();
            if (current_sp > call_frame.saved_sp) {
                const src_index: usize = @intCast(current_sp - 1);
                const dst_index: usize = @intCast(call_frame.saved_sp);
                vm.stack.data[dst_index] = vm.stack.data[src_index];
                vm.stack.sp = call_frame.saved_sp + 1;
            } else {
                vm.stack.sp = call_frame.saved_sp;
                if (payload.has_value) {
                    return vm.reporter.reportRuntimeError(null, ErrorCode.INTERNAL_ERROR, "Return has value but stack is empty", .{});
                } else {
                    try vm.stack.push(HIRFrame.initNothing());
                }
            }

            vm.ip = call_frame.return_ip;

            return;
        }
    }

    fn execBuiltinFunction(vm: anytype, c: anytype) !void {
        if (std.mem.eql(u8, c.qualified_name, "length")) {
            const array = try vm.stack.pop();
            switch (array.value) {
                .array => |arr| {
                    var length: u32 = 0;
                    for (arr.elements) |elem| {
                        if (elem.* == .nothing) break;
                        length += 1;
                    }
                    try vm.stack.push(HIRFrame.initInt(@as(i64, @intCast(length))));
                },
                else => return vm.reporter.reportRuntimeError(null, ErrorCode.VARIABLE_NOT_FOUND, "Cannot get length of non-array value: {s}", .{@tagName(array.value)}),
            }
        } else if (std.mem.eql(u8, c.qualified_name, "push")) {
            try execBuiltinArrayPush(vm);
        } else if (std.mem.eql(u8, c.qualified_name, "power") or std.mem.eql(u8, c.qualified_name, "powi")) {
            try execBuiltinPower(vm, c.qualified_name);
        } else if (std.mem.eql(u8, c.qualified_name, "exists_quantifier_gt") or
            std.mem.eql(u8, c.qualified_name, "exists_quantifier_eq") or
            std.mem.eql(u8, c.qualified_name, "forall_quantifier_gt") or
            std.mem.eql(u8, c.qualified_name, "forall_quantifier_eq"))
        {
            // Quantifier functions are handled directly in VM's execBuiltin function
            return vm.reporter.reportRuntimeError(null, ErrorCode.VARIABLE_NOT_FOUND, "Quantifier functions should be handled by VM", .{});
        } else if (std.mem.eql(u8, c.qualified_name, "exit")) {
            try execBuiltinExit(vm);
        } else if (std.mem.eql(u8, c.qualified_name, "clear")) {
            try execBuiltinClear(vm);
        } else if (std.mem.eql(u8, c.qualified_name, "find")) {
            try execBuiltinFind(vm);
        } else {
            return vm.reporter.reportRuntimeError(null, ErrorCode.VARIABLE_NOT_FOUND, "Unknown built-in function: {s}", .{c.qualified_name});
        }
    }

    fn execModuleFunction(vm: anytype, c: anytype) !void {
        // Try to find the function by exact qualified name in the function table.
        // This covers all module functions (e.g. "safeMath.safeAdd", "math.add").
        for (vm.program.function_table) |func| {
            if (std.mem.eql(u8, func.name, c.qualified_name) or
                std.mem.eql(u8, func.qualified_name, c.qualified_name))
            {
                return dispatchModuleCall(vm, func, c);
            }
        }

        // For un-dotted names, also try well-known render-module prefixes.
        if (std.mem.indexOfScalar(u8, c.qualified_name, '.') == null) {
            const function_name = c.qualified_name;
            const prefixes = [_][]const u8{ "Render.", "render.", "Render.doxa.", "render.doxa." };
            for (prefixes) |prefix| {
                const qualified_name = try std.fmt.allocPrint(vm.runtimeAllocator(), "{s}{s}", .{ prefix, function_name });
                for (vm.program.function_table) |func| {
                    if (std.mem.eql(u8, func.name, qualified_name)) {
                        return dispatchModuleCall(vm, func, c);
                    }
                }
            }
        }

        return vm.reporter.reportRuntimeError(null, ErrorCode.VARIABLE_NOT_FOUND, "Unknown module function: {s}", .{c.qualified_name});
    }

    /// Helper: set up a call frame and jump to a resolved module function.
    fn dispatchModuleCall(vm: anytype, function: anytype, c: anytype) !void {
        const saved_sp = vm.stack.size() - @as(i64, @intCast(c.arg_count));
        const return_ip = vm.ip + 1;
        const call_frame = CallFrame{
            .return_ip = return_ip,
            .function_name = function.name,
            .arg_count = c.arg_count,
            .saved_sp = saved_sp,
        };
        try vm.call_stack.push(call_frame);

        if (function.start_ip != 0) {
            vm.ip = function.start_ip;
            return;
        }
        return vm.reporter.reportRuntimeError(null, ErrorCode.VARIABLE_NOT_FOUND, "Function label not resolved: {s}", .{function.start_label});
    }

    fn execBuiltinArrayPush(vm: anytype) !void {
        const element = try vm.stack.pop();
        const array = try vm.stack.pop();

        switch (array.value) {
            .array => |arr| {
                var mutable_arr = arr;

                var length: u32 = 0;
                for (mutable_arr.elements) |elem| {
                    if (std.meta.eql(elem, HIRValue.nothing)) break;
                    length += 1;
                }

                if (length >= mutable_arr.capacity) {
                    const new_capacity = mutable_arr.capacity * 2;
                    const new_elements = try vm.runtimeAllocator().realloc(mutable_arr.elements, new_capacity);
                    mutable_arr.elements = new_elements;
                    mutable_arr.capacity = new_capacity;

                    for (length..new_capacity) |i| {
                        mutable_arr.elements[i] = HIRValue.nothing;
                    }
                }

                mutable_arr.elements[length] = element.value;

                const modified_array_value = HIRValue{ .array = mutable_arr };
                try vm.stack.push(HIRFrame.initFromHIRValue(modified_array_value));
            },
            else => return vm.reporter.reportRuntimeError(null, ErrorCode.VARIABLE_NOT_FOUND, "Cannot push to non-array value: {s}", .{@tagName(array.value)}),
        }
    }

    fn execBuiltinClear(vm: anytype) !void {
        const coll = try vm.stack.pop();
        switch (coll.value) {
            .array => |arr| {
                const mutable_arr = arr;
                for (mutable_arr.elements) |*elem| {
                    elem.* = HIRValue.nothing;
                }
                // Clear modifies in place and returns nothing
            },
            .string => {
                // Strings are immutable, so clear on string should report an error
                return vm.reporter.reportRuntimeError(
                    null,
                    ErrorCode.INVALID_ARRAY_TYPE,
                    "clear: cannot clear string (strings are immutable)",
                    .{},
                );
            },
            else => return vm.reporter.reportRuntimeError(
                null,
                ErrorCode.INVALID_ARRAY_TYPE,
                "clear: target must be array, got {s}",
                .{@tagName(coll.value)},
            ),
        }
    }

    fn valuesEqual(a: HIRValue, b: HIRValue) bool {
        return switch (a) {
            .int => |av| switch (b) {
                .int => |bv| av == bv,
                .byte => |bv| av == bv,
                .float => |bv| @as(f64, @floatFromInt(av)) == bv,
                else => false,
            },
            .float => |av| switch (b) {
                .float => |bv| av == bv,
                .byte => |bv| av == @as(f64, @floatFromInt(bv)),
                .int => |bv| av == @as(f64, @floatFromInt(bv)),
                else => false,
            },
            .byte => |av| switch (b) {
                .byte => |bv| av == bv,
                .int => |bv| av == bv,
                .float => |bv| @as(f64, @floatFromInt(av)) == bv,
                else => false,
            },
            .tetra => |av| switch (b) {
                .tetra => |bv| av == bv,
                else => false,
            },
            .string => |av| switch (b) {
                .string => |bv| std.mem.eql(u8, av, bv),
                else => false,
            },
            .nothing => switch (b) {
                .nothing => true,
                else => false,
            },
            .enum_variant => |av| switch (b) {
                .enum_variant => |bv| av.variant_index == bv.variant_index,
                else => false,
            },
            else => false,
        };
    }

    fn execBuiltinFind(vm: anytype) !void {
        const value = try vm.stack.pop();
        const coll = try vm.stack.pop();

        switch (coll.value) {
            .array => |arr| {
                var idx: i64 = 0;
                for (arr.elements) |elem| {
                    if (std.meta.eql(elem, HIRValue.nothing)) break;
                    if (valuesEqual(elem, value.value)) {
                        try vm.stack.push(HIRFrame.initInt(idx));
                        return;
                    }
                    idx += 1;
                }
                try vm.stack.push(HIRFrame.initInt(-1));
            },
            .string => |s_val| {
                const needle = switch (value.value) {
                    .string => |s| s,
                    else => {
                        return vm.reporter.reportRuntimeError(
                            null,
                            ErrorCode.TYPE_MISMATCH,
                            "@find on string requires string value, got {s}",
                            .{@tagName(value.value)},
                        );
                    },
                };

                if (needle.len == 0) {
                    try vm.stack.push(HIRFrame.initInt(0));
                    return;
                }

                const maybe_idx = std.mem.indexOf(u8, s_val, needle);
                if (maybe_idx) |found| {
                    try vm.stack.push(HIRFrame.initInt(@intCast(found)));
                } else {
                    try vm.stack.push(HIRFrame.initInt(-1));
                }
            },
            else => return vm.reporter.reportRuntimeError(
                null,
                ErrorCode.INVALID_ARRAY_TYPE,
                "@find requires array or string, got {s}",
                .{@tagName(coll.value)},
            ),
        }
    }

    fn execBuiltinPower(vm: anytype, function_name: []const u8) !void {
        const exponent = try vm.stack.pop();
        const base = try vm.stack.pop();

        if (std.mem.eql(u8, function_name, "powi")) {
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
            const result = std.math.pow(i64, base_int, @as(u32, @intCast(exp_int)));
            try vm.stack.push(HIRFrame.initInt(result));
            return;
        }

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

    fn execBuiltinExit(vm: anytype) !void {
        const exit_code_frame = try vm.stack.pop();
        _ = switch (exit_code_frame.value) {
            .int => |i| @as(u8, @intCast(i & 0xFF)),
            .byte => |b| b,
            else => {
                return vm.reporter.reportRuntimeError(null, ErrorCode.VARIABLE_NOT_FOUND, "@exit: argument must be an integer", .{});
            },
        };

        vm.running = false;
    }

};
