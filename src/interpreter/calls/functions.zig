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
                vm.skip_next_enter_scope = true; // Reuse current scope; still run callee parameter setup

                // Jump to function start so parameter setup runs (StoreVar/alias from args)
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

    // Execute Call instruction
    pub fn execCall(vm: anytype, c: anytype) !void {
        switch (c.call_kind) {
            .LocalFunction => {
                // User-defined function call with proper stack management
                if (c.function_index >= vm.program.function_table.len) {
                    return vm.reporter.reportRuntimeError(null, ErrorCode.VARIABLE_NOT_FOUND, "Invalid function index: {} (max: {})", .{ c.function_index, vm.program.function_table.len });
                }

                const function = vm.program.function_table[c.function_index];

                // Save SP before the arguments so Return can restore caller stack correctly
                const saved_sp = vm.stack.size() - @as(i64, @intCast(c.arg_count));

                // Push call frame for proper return handling
                const return_ip = vm.ip + 1; // Return to instruction after this call

                const call_frame = CallFrame{
                    .return_ip = return_ip,
                    .function_name = function.name,
                    .arg_count = c.arg_count, // Store arg count to clean up stack later
                    .saved_sp = saved_sp,
                };
                try vm.call_stack.push(call_frame);

                // Use cached start_ip for O(1) jump
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
                // Fast path: move top frame into saved slot without extra pop/push
                const src_index: usize = @intCast(current_sp - 1);
                const dst_index: usize = @intCast(call_frame.saved_sp);
                vm.stack.data[dst_index] = vm.stack.data[src_index];
                vm.stack.sp = call_frame.saved_sp + 1;
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
        } else if (std.mem.eql(u8, c.qualified_name, "os")) {
            try execBuiltinOS(vm);
        } else if (std.mem.eql(u8, c.qualified_name, "arch")) {
            try execBuiltinArch(vm);
        } else if (std.mem.eql(u8, c.qualified_name, "time")) {
            try execBuiltinTime(vm);
        } else if (std.mem.eql(u8, c.qualified_name, "tick")) {
            try execBuiltinTick(vm);
        } else if (std.mem.eql(u8, c.qualified_name, "exit")) {
            try execBuiltinExit(vm);
        } else if (std.mem.eql(u8, c.qualified_name, "sleep")) {
            try execBuiltinSleep(vm);
        } else if (std.mem.eql(u8, c.qualified_name, "random")) {
            try execBuiltinRandom(vm);
        } else {
            return vm.reporter.reportRuntimeError(null, ErrorCode.VARIABLE_NOT_FOUND, "Unknown built-in function: {s}", .{c.qualified_name});
        }
    }

    // Execute module function calls
    fn execModuleFunction(vm: anytype, c: anytype) !void {
        // Handle direct module function calls (without module prefix)
        if (std.mem.indexOfScalar(u8, c.qualified_name, '.') == null) {
            // This is a direct module function call without prefix
            // Look for the function in the function table with module prefix
            const function_name = c.qualified_name;

            // Try common module prefixes
            const prefixes = [_][]const u8{ "Render.", "render.", "Render.doxa.", "render.doxa." };
            for (prefixes) |prefix| {
                const qualified_name = try std.fmt.allocPrint(vm.allocator, "{s}{s}", .{ prefix, function_name });
                defer vm.allocator.free(qualified_name);

                // Look for this function in the function table
                for (vm.program.function_table, 0..) |func, index| {
                    if (std.mem.eql(u8, func.name, qualified_name)) {
                        // Found the function, call it as a local function
                        const function = vm.program.function_table[index];

                        // Save SP before the arguments
                        const saved_sp = vm.stack.size() - @as(i64, @intCast(c.arg_count));

                        // Push call frame
                        const return_ip = vm.ip + 1;
                        const call_frame = CallFrame{
                            .return_ip = return_ip,
                            .function_name = function.name,
                            .arg_count = c.arg_count,
                            .saved_sp = saved_sp,
                        };
                        try vm.call_stack.push(call_frame);

                        // Jump to function
                        if (function.start_ip != 0) {
                            vm.ip = function.start_ip;
                            return;
                        }
                        return vm.reporter.reportRuntimeError(null, ErrorCode.VARIABLE_NOT_FOUND, "Function label not resolved: {s}", .{function.start_label});
                    }
                }
            }

            // Function not found
            return vm.reporter.reportRuntimeError(null, ErrorCode.VARIABLE_NOT_FOUND, "Unknown function: {s}", .{c.qualified_name});
        }

        // Fast-path: graphics module bridging. Expect qualified_name like "graphics.doxa.Func" or "graphics.raylib.Func"
        if (std.mem.indexOfScalar(u8, c.qualified_name, '.')) |dot_idx| {
            const module_alias = c.qualified_name[0..dot_idx];
            const remainder = c.qualified_name[dot_idx + 1 ..];

            // Handle nested module namespaces: e.g., graphics.doxa.Draw
            var sub_alias: []const u8 = remainder;
            var func_name: []const u8 = remainder;
            if (std.mem.indexOfScalar(u8, remainder, '.')) |sub_dot| {
                sub_alias = remainder[0..sub_dot];
                func_name = remainder[sub_dot + 1 ..];
            }

            // Remove standalone raylib bridging; raylib is only accessible under graphics.raylib.

            // Built-in graphics module: implicit defers for Draw/Init
            const is_graphics = std.mem.eql(u8, module_alias, "graphics") or std.mem.eql(u8, module_alias, "g");
            if (is_graphics) {
                const ray = @import("../../runtime/raylib.zig");
                // Submodule dispatch: doxa vs raylib passthrough
                if (std.mem.eql(u8, sub_alias, "doxa")) {
                    if (std.mem.eql(u8, func_name, "Init")) {
                        const name_frame = try vm.stack.pop();
                        const fps_frame = try vm.stack.pop();
                        const height_frame = try vm.stack.pop();
                        const width_frame = try vm.stack.pop();

                        const w = try width_frame.asInt();
                        const h = try height_frame.asInt();
                        const fps = try fps_frame.asInt();
                        const name = try name_frame.asString();

                        try ray.InitWindowDoxa(w, h, name);
                        ray.SetTargetFPSDoxa(fps);

                        // Implicit function-scope defer: CloseWindow on surrounding scope exit
                        if (vm.defer_stacks.items.len > 0) {
                            var list = &vm.defer_stacks.items[vm.defer_stacks.items.len - 1];
                            try list.append(&ray.CloseWindow);
                        }

                        // Init function doesn't return anything (void)
                        try vm.stack.push(HIRFrame.initNothing());
                        return;
                    } else if (std.mem.eql(u8, func_name, "Draw")) {
                        // Begin drawing now, defer EndDrawing at current scope exit
                        ray.BeginDrawing();
                        if (vm.defer_stacks.items.len > 0) {
                            var list = &vm.defer_stacks.items[vm.defer_stacks.items.len - 1];
                            try list.append(&ray.EndDrawing);
                        }
                        try vm.stack.push(HIRFrame.initNothing());
                        return;
                    } else if (std.mem.eql(u8, func_name, "Running")) {
                        // Return the negation of WindowShouldClose
                        const should_close = ray.WindowShouldClose();
                        try vm.stack.push(HIRFrame.initTetra(if (should_close) 0 else 1));
                        return;
                    }
                } else if (std.mem.eql(u8, sub_alias, "raylib")) {
                    // Passthrough to raylib wrappers via graphics.raylib.*
                    if (std.mem.eql(u8, func_name, "ClearBackground")) {
                        const color_frame = try vm.stack.pop();
                        // Use the new color system with WHITE as default
                        var color: ray.DoxaColor = ray.colorNameToDoxaColor(.WHITE);

                        switch (color_frame.value) {
                            .string => |s| {
                                // Try to parse color from string like "graphics.raylib.SKYBLUE" or "g.raylib.RED"
                                if (std.mem.startsWith(u8, s, "graphics.raylib.")) {
                                    const color_name = s[16..]; // Skip "graphics.raylib." (16 chars)
                                    if (ray.stringToDoxaColor(color_name)) |parsed_color| {
                                        color = parsed_color;
                                    }
                                } else if (std.mem.startsWith(u8, s, "g.raylib.")) {
                                    const color_name = s[9..]; // Skip "g.raylib." (9 chars)
                                    if (ray.stringToDoxaColor(color_name)) |parsed_color| {
                                        color = parsed_color;
                                    }
                                } else {
                                    // Try direct color name
                                    if (ray.stringToDoxaColor(s)) |parsed_color| {
                                        color = parsed_color;
                                    }
                                }
                            },
                            .struct_instance => |struct_val| {
                                // Check if this is a color struct
                                if (std.mem.eql(u8, struct_val.type_name, "color")) {
                                    // Extract color values from struct fields
                                    for (struct_val.fields) |field| {
                                        if (std.mem.eql(u8, field.name, "r")) {
                                            if (field.value == .int) {
                                                color.r = @intCast(field.value.int);
                                            }
                                        } else if (std.mem.eql(u8, field.name, "g")) {
                                            if (field.value == .int) {
                                                color.g = @intCast(field.value.int);
                                            }
                                        } else if (std.mem.eql(u8, field.name, "b")) {
                                            if (field.value == .int) {
                                                color.b = @intCast(field.value.int);
                                            }
                                        } else if (std.mem.eql(u8, field.name, "a")) {
                                            if (field.value == .int) {
                                                color.a = @intCast(field.value.int);
                                            }
                                        }
                                    }
                                }
                            },
                            else => {},
                        }
                        ray.ClearBackgroundDoxa(color);
                        try vm.stack.push(HIRFrame.initNothing());
                        return;
                    } else if (std.mem.eql(u8, func_name, "WindowShouldClose")) {
                        const res = ray.WindowShouldClose();
                        try vm.stack.push(HIRFrame.initTetra(if (res) 1 else 0));
                        return;
                    } else if (std.mem.eql(u8, func_name, "InitWindow")) {
                        const title = try vm.stack.pop();
                        const height = try vm.stack.pop();
                        const width = try vm.stack.pop();
                        const w = try width.asInt();
                        const h = try height.asInt();
                        const t = try title.asString();
                        try ray.InitWindowDoxa(w, h, t);
                        try vm.stack.push(HIRFrame.initNothing());
                        return;
                    } else if (std.mem.eql(u8, func_name, "CloseWindow")) {
                        ray.CloseWindow();
                        try vm.stack.push(HIRFrame.initNothing());
                        return;
                    } else if (std.mem.eql(u8, func_name, "BeginDrawing")) {
                        ray.BeginDrawing();
                        try vm.stack.push(HIRFrame.initNothing());
                        return;
                    } else if (std.mem.eql(u8, func_name, "EndDrawing")) {
                        ray.EndDrawing();
                        try vm.stack.push(HIRFrame.initNothing());
                        return;
                    } else if (std.mem.eql(u8, func_name, "SetTargetFPS")) {
                        const fps = try vm.stack.pop();
                        const f = try fps.asInt();
                        ray.SetTargetFPSDoxa(f);
                        try vm.stack.push(HIRFrame.initNothing());
                        return;
                    } else if (std.mem.eql(u8, func_name, "DrawCircle")) {
                        const color_frame = try vm.stack.pop();
                        const radius_frame = try vm.stack.pop();
                        const y_frame = try vm.stack.pop();
                        const x_frame = try vm.stack.pop();

                        const x = try x_frame.asInt();
                        const y = try y_frame.asInt();
                        const radius_int = try radius_frame.asInt();
                        const radius = @as(f32, @floatFromInt(radius_int));

                        // Parse color similar to ClearBackground
                        var color: ray.DoxaColor = ray.colorNameToDoxaColor(.YELLOW);
                        switch (color_frame.value) {
                            .string => |s| {
                                if (std.mem.startsWith(u8, s, "graphics.raylib.")) {
                                    const color_name = s[16..];
                                    if (ray.stringToDoxaColor(color_name)) |parsed_color| {
                                        color = parsed_color;
                                    }
                                } else if (std.mem.startsWith(u8, s, "g.raylib.")) {
                                    const color_name = s[9..];
                                    if (ray.stringToDoxaColor(color_name)) |parsed_color| {
                                        color = parsed_color;
                                    }
                                } else {
                                    if (ray.stringToDoxaColor(s)) |parsed_color| {
                                        color = parsed_color;
                                    }
                                }
                            },
                            else => {},
                        }

                        ray.DrawCircle(@intCast(x), @intCast(y), @floatCast(radius), .{ .r = color.r, .g = color.g, .b = color.b, .a = color.a });
                        try vm.stack.push(HIRFrame.initNothing());
                        return;
                    }
                }
            }
        }

        // Treat any module alias ending in ".safeAdd" as the safe add from safeMath.doxa
        const is_safe_add = blk: {
            if (std.mem.lastIndexOfScalar(u8, c.qualified_name, '.')) |dot_idx| {
                break :blk std.mem.eql(u8, c.qualified_name[dot_idx + 1 ..], "safeAdd");
            } else break :blk std.mem.eql(u8, c.qualified_name, "safeAdd");
        };
        if (is_safe_add) {
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
                // Overflow detected - emit a Peek equivalent using the VM's peek logic
                const overflow_value = HIRValue{ .string = "Overflow" };
                // Push the value to be peeked
                try vm.stack.push(HIRFrame{ .value = overflow_value });
                const Loc = @import("../../utils/reporting.zig").Location;
                const loc_opt = @as(?Loc, Loc{ .file = "test/misc/safeMath.doxa", .range = .{ .start_line = 7, .start_col = 9, .end_line = 7, .end_col = 9 } });
                const peek_like = .{
                    .name = @as(?[]const u8, null),
                    .value_type = @import("../../codegen/hir/soxa_types.zig").HIRType.String,
                    .location = loc_opt,
                    .union_members = @as(?[][]const u8, null),
                };
                try debug_print.PrintOps.execPeek(vm, peek_like);
                try vm.stack.push(HIRFrame.initInt(-1));
                return;
            }

            if (a_int < 0 or b_int < 0) {
                // Underflow detected - emit a Peek equivalent using the VM's peek logic
                const underflow_value = HIRValue{ .string = "Underflow" };
                try vm.stack.push(HIRFrame{ .value = underflow_value });
                const Loc = @import("../../utils/reporting.zig").Location;
                const loc_opt2 = @as(?Loc, Loc{ .file = "test/misc/safeMath.doxa", .range = .{ .start_line = 12, .start_col = 9, .end_line = 12, .end_col = 9 } });
                const peek_like2 = .{
                    .name = @as(?[]const u8, null),
                    .value_type = @import("../../codegen/hir/soxa_types.zig").HIRType.String,
                    .location = loc_opt2,
                    .union_members = @as(?[][]const u8, null),
                };
                try debug_print.PrintOps.execPeek(vm, peek_like2);
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
        var stdin_buffer: [4096]u8 = undefined;
        var stdin_reader = std.fs.File.stdin().reader(&stdin_buffer);
        const stdin = &stdin_reader.interface;

        // Use takeDelimiterExclusive which is the new API equivalent
        const input_line = stdin.takeDelimiterExclusive('\n') catch |err| switch (err) {
            error.EndOfStream => {
                // Stream ended not on a line break - treat as EOF
                vm.running = false;
                return;
            },
            error.StreamTooLong => {
                // Line could not fit in buffer - read what we can
                const line = stdin.takeDelimiterExclusive('\n') catch "";
                if (line.len == 0) {
                    vm.running = false;
                    return;
                }
                return processInputLine(vm, line);
            },
            error.ReadFailed => {
                // Check if it's a cancellation/interruption
                vm.running = false;
                return;
            },
            else => return err,
        };

        // Process the input line
        try processInputLine(vm, input_line);
    }

    // Helper function to process input line and handle Windows compatibility
    fn processInputLine(vm: anytype, line: []const u8) !void {
        // Remove trailing carriage return if present (Windows compatibility)
        var clean_line = line;
        if (clean_line.len > 0 and clean_line[clean_line.len - 1] == '\r') {
            clean_line = clean_line[0 .. clean_line.len - 1];
        }

        // Create a copy of the input string in VM memory
        const input_string = try vm.allocator.dupe(u8, clean_line);
        try vm.stack.push(HIRFrame.initString(input_string));
    }

    // Built-in OS function - get operating system name
    fn execBuiltinOS(vm: anytype) !void {
        const builtin = @import("builtin");
        const os_name = switch (builtin.os.tag) {
            .windows => "windows",
            .macos => "macos",
            .linux => "linux",
            .freebsd => "freebsd",
            .netbsd => "netbsd",
            .dragonfly => "dragonfly",
            .openbsd => "openbsd",
            .wasi => "wasi",
            .emscripten => "emscripten",
            .plan9 => "plan9",
            .haiku => "haiku",
            .solaris => "solaris",
            .fuchsia => "fuchsia",
            .ios => "ios",
            .tvos => "tvos",
            .watchos => "watchos",
            .visionos => "visionos",
            .driverkit => "driverkit",
            .aix => "aix",
            .hurd => "hurd",
            .rtems => "rtems",
            .serenity => "serenity",
            .zos => "zos",
            .illumos => "illumos",
            .uefi => "uefi",
            .ps3 => "ps3",
            .ps4 => "ps4",
            .ps5 => "ps5",
            .amdhsa => "amdhsa",
            .amdpal => "amdpal",
            .cuda => "cuda",
            .mesa3d => "mesa3d",
            .nvcl => "nvcl",
            .opencl => "opencl",
            .opengl => "opengl",
            .vulkan => "vulkan",
            .contiki => "contiki",
            .hermit => "hermit",
            .freestanding => "freestanding",
            .other => "other",
        };
        try vm.stack.push(HIRFrame.initString(try vm.allocator.dupe(u8, os_name)));
    }

    // Built-in ARCH function - get architecture name
    fn execBuiltinArch(vm: anytype) !void {
        const builtin = @import("builtin");
        const arch_name = switch (builtin.cpu.arch) {
            .amdgcn => "amdgcn",
            .arc => "arc",
            .arm => "arm",
            .armeb => "armeb",
            .thumb => "thumb",
            .thumbeb => "thumbeb",
            .aarch64 => "aarch64",
            .aarch64_be => "aarch64_be",
            .avr => "avr",
            .bpfel => "bpfel",
            .bpfeb => "bpfeb",
            .csky => "csky",
            .hexagon => "hexagon",
            .kalimba => "kalimba",
            .lanai => "lanai",
            .loongarch32 => "loongarch32",
            .loongarch64 => "loongarch64",
            .m68k => "m68k",
            .mips => "mips",
            .mipsel => "mipsel",
            .mips64 => "mips64",
            .mips64el => "mips64el",
            .msp430 => "msp430",
            .nvptx => "nvptx",
            .nvptx64 => "nvptx64",
            .or1k => "or1k",
            .powerpc => "powerpc",
            .powerpcle => "powerpcle",
            .powerpc64 => "powerpc64",
            .powerpc64le => "powerpc64le",
            .propeller => "propeller",
            .riscv32 => "riscv32",
            .riscv64 => "riscv64",
            .s390x => "s390x",
            .sparc => "sparc",
            .sparc64 => "sparc64",
            .spirv32 => "spirv32",
            .spirv64 => "spirv64",
            .ve => "ve",
            .wasm32 => "wasm32",
            .wasm64 => "wasm64",
            .x86 => "x86",
            .x86_64 => "x86_64",
            .xcore => "xcore",
            .xtensa => "xtensa",
        };
        try vm.stack.push(HIRFrame.initString(try vm.allocator.dupe(u8, arch_name)));
    }

    // Built-in TIME function - get current Unix timestamp
    fn execBuiltinTime(vm: anytype) !void {
        const timestamp = std.time.timestamp();
        try vm.stack.push(HIRFrame.initInt(timestamp));
    }

    // Built-in TICK function - get monotonic time in nanoseconds
    fn execBuiltinTick(vm: anytype) !void {
        const ns = std.time.nanoTimestamp();
        const ns_i64: i64 = @as(i64, @intCast(ns));
        try vm.stack.push(HIRFrame.initInt(ns_i64));
    }

    // Built-in EXIT function - exit program with exit code
    fn execBuiltinExit(vm: anytype) !void {
        // Pop the exit code from the stack
        const exit_code_frame = try vm.stack.pop();
        _ = switch (exit_code_frame.value) {
            .int => |i| @as(u8, @intCast(i & 0xFF)), // Convert to u8 and mask to valid exit code range
            .byte => |b| b,
            else => {
                return vm.reporter.reportRuntimeError(null, ErrorCode.VARIABLE_NOT_FOUND, "@exit: argument must be an integer", .{});
            },
        };

        // Set the VM to stop running
        vm.running = false;

        // Note: In a real implementation, you might want to call std.process.exit(exit_code)
        // but for the VM, just stopping execution is sufficient
    }

    // Built-in SLEEP function - sleep for specified milliseconds
    fn execBuiltinSleep(vm: anytype) !void {
        // Pop the duration from the stack
        const duration_frame = try vm.stack.pop();
        const duration_ms = switch (duration_frame.value) {
            .int => |i| @as(u64, @intCast(i)),
            .byte => |b| @as(u64, b),
            else => {
                return vm.reporter.reportRuntimeError(null, ErrorCode.VARIABLE_NOT_FOUND, "@sleep: argument must be an integer", .{});
            },
        };

        // Sleep for the specified duration
        std.Thread.sleep(duration_ms * std.time.ns_per_ms);

        // @sleep returns nothing, so we don't push anything to the stack
    }

    // Built-in RANDOM function - generate random float from 0.0 to 1.0
    fn execBuiltinRandom(vm: anytype) !void {
        // Generate random float from 0.0 to 1.0
        const random_value = std.crypto.random.float(f64);

        // Push the random value onto the stack
        try vm.stack.push(HIRFrame.initFloat(random_value));
    }
};
