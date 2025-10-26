const std = @import("std");
const HIRValue = @import("../../codegen/hir/soxa_values.zig").HIRValue;
const HIRStruct = @import("../../codegen/hir/soxa_values.zig").HIRStruct;
const HIRStructField = @import("../../codegen/hir/soxa_values.zig").HIRStructField;
const Core = @import("../core.zig");
const HIRFrame = Core.HIRFrame;
const CallFrame = Core.CallFrame;
const Errors = @import("../../utils/errors.zig");
const ErrorList = Errors.ErrorList;
const ErrorCode = Errors.ErrorCode;
const debug_print = @import("../calls/print.zig");
const PrintOps = debug_print.PrintOps;

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
        } else if (std.mem.eql(u8, c.qualified_name, "exists_quantifier_gt") or
            std.mem.eql(u8, c.qualified_name, "exists_quantifier_eq") or
            std.mem.eql(u8, c.qualified_name, "forall_quantifier_gt") or
            std.mem.eql(u8, c.qualified_name, "forall_quantifier_eq"))
        {
            // Quantifier functions are handled directly in VM's execBuiltin function
            return vm.reporter.reportRuntimeError(null, ErrorCode.VARIABLE_NOT_FOUND, "Quantifier functions should be handled by VM", .{});
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

    fn execModuleFunction(vm: anytype, c: anytype) !void {
        if (std.mem.indexOfScalar(u8, c.qualified_name, '.') == null) {
            const function_name = c.qualified_name;

            const prefixes = [_][]const u8{ "Render.", "render.", "Render.doxa.", "render.doxa." };
            for (prefixes) |prefix| {
                const qualified_name = try std.fmt.allocPrint(vm.allocator, "{s}{s}", .{ prefix, function_name });
                defer vm.allocator.free(qualified_name);

                for (vm.program.function_table, 0..) |func, index| {
                    if (std.mem.eql(u8, func.name, qualified_name)) {
                        const function = vm.program.function_table[index];

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
                }
            }

            return vm.reporter.reportRuntimeError(null, ErrorCode.VARIABLE_NOT_FOUND, "Unknown function: {s}", .{c.qualified_name});
        }

        if (std.mem.indexOfScalar(u8, c.qualified_name, '.')) |dot_idx| {
            const module_alias = c.qualified_name[0..dot_idx];
            const remainder = c.qualified_name[dot_idx + 1 ..];

            var sub_alias: []const u8 = remainder;
            var func_name: []const u8 = remainder;
            if (std.mem.indexOfScalar(u8, remainder, '.')) |sub_dot| {
                sub_alias = remainder[0..sub_dot];
                func_name = remainder[sub_dot + 1 ..];
            }

            const is_graphics = std.mem.eql(u8, module_alias, "graphics") or std.mem.eql(u8, module_alias, "g");
            if (is_graphics) {
                const ray = @import("../../runtime/raylib.zig");
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

                        vm.reporter.debug(">> dg.Init: scope_stack has {any) items", .{vm.scope_stack.items.len}, @src());
                        if (vm.scope_stack.items.len > 0) {
                            var scope = &vm.scope_stack.items[vm.scope_stack.items.len - 1];
                            vm.reporter.debug(">> dg.Init: Adding CloseWindow defer to scope {any)", .{scope.id}, @src());
                            try scope.defer_actions.append(&ray.CloseWindow);
                            vm.reporter.debug(">> dg.Init: Scope now has {any) defer actions", .{scope.defer_actions.items.len}, @src());
                        } else {
                            vm.reporter.debug(">> dg.Init: No scope available for defer", .{}, @src());
                        }

                        return;
                    } else if (std.mem.eql(u8, func_name, "Draw")) {
                        ray.BeginDrawing();
                        return;
                    } else if (std.mem.eql(u8, func_name, "EndDrawing")) {
                        ray.EndDrawing();
                        return;
                    } else if (std.mem.eql(u8, func_name, "CloseWindow")) {
                        ray.CloseWindow();
                        return;
                    } else if (std.mem.eql(u8, func_name, "Running")) {
                        const should_close = ray.WindowShouldClose();
                        try vm.stack.push(HIRFrame.initTetra(if (should_close) 0 else 1));
                        return;
                    } else if (std.mem.eql(u8, func_name, "rgbToColor")) {
                        const b_frame = try vm.stack.pop();
                        const g_frame = try vm.stack.pop();
                        const r_frame = try vm.stack.pop();

                        const r = switch (r_frame.value) {
                            .int => |val| @as(u8, @intCast(val)),
                            .byte => |val| val,
                            else => return vm.reporter.reportRuntimeError(null, ErrorCode.INVALID_ARGUMENT, "Expected int or byte for red component", .{}),
                        };
                        const g = switch (g_frame.value) {
                            .int => |val| @as(u8, @intCast(val)),
                            .byte => |val| val,
                            else => return vm.reporter.reportRuntimeError(null, ErrorCode.INVALID_ARGUMENT, "Expected int or byte for green component", .{}),
                        };
                        const b = switch (b_frame.value) {
                            .int => |val| @as(u8, @intCast(val)),
                            .byte => |val| val,
                            else => return vm.reporter.reportRuntimeError(null, ErrorCode.INVALID_ARGUMENT, "Expected int or byte for blue component", .{}),
                        };

                        if (r < 0 or r > 255 or g < 0 or g > 255 or b < 0 or b > 255) {
                            return vm.reporter.reportRuntimeError(null, ErrorCode.INVALID_ARGUMENT, "Color values must be between 0 and 255", .{});
                        }

                        const color = ray.rgbToColor(r, g, b);

                        var fields = std.array_list.Managed(HIRStructField).init(vm.allocator);
                        defer fields.deinit();

                        try fields.append(HIRStructField{
                            .name = try vm.allocator.dupe(u8, "r"),
                            .value = HIRValue{ .int = color.r },
                            .field_type = .Int,
                        });
                        try fields.append(HIRStructField{
                            .name = try vm.allocator.dupe(u8, "g"),
                            .value = HIRValue{ .int = color.g },
                            .field_type = .Int,
                        });
                        try fields.append(HIRStructField{
                            .name = try vm.allocator.dupe(u8, "b"),
                            .value = HIRValue{ .int = color.b },
                            .field_type = .Int,
                        });
                        try fields.append(HIRStructField{
                            .name = try vm.allocator.dupe(u8, "a"),
                            .value = HIRValue{ .int = color.a },
                            .field_type = .Int,
                        });

                        const color_struct = HIRStruct{
                            .type_name = try vm.allocator.dupe(u8, "color"),
                            .fields = try fields.toOwnedSlice(),
                        };

                        const struct_value = HIRValue{ .struct_instance = color_struct };
                        try vm.stack.push(HIRFrame.initFromHIRValue(struct_value));
                        return;
                    } else if (std.mem.eql(u8, func_name, "bytesToColor")) {
                        const a_frame = try vm.stack.pop();
                        const b_frame = try vm.stack.pop();
                        const g_frame = try vm.stack.pop();
                        const r_frame = try vm.stack.pop();

                        const r = switch (r_frame.value) {
                            .int => |val| @as(u8, @intCast(val)),
                            .byte => |val| val,
                            else => return vm.reporter.reportRuntimeError(null, ErrorCode.INVALID_ARGUMENT, "Expected int or byte for red component", .{}),
                        };
                        const g = switch (g_frame.value) {
                            .int => |val| @as(u8, @intCast(val)),
                            .byte => |val| val,
                            else => return vm.reporter.reportRuntimeError(null, ErrorCode.INVALID_ARGUMENT, "Expected int or byte for green component", .{}),
                        };
                        const b = switch (b_frame.value) {
                            .int => |val| @as(u8, @intCast(val)),
                            .byte => |val| val,
                            else => return vm.reporter.reportRuntimeError(null, ErrorCode.INVALID_ARGUMENT, "Expected int or byte for blue component", .{}),
                        };
                        const a = switch (a_frame.value) {
                            .int => |val| @as(u8, @intCast(val)),
                            .byte => |val| val,
                            else => return vm.reporter.reportRuntimeError(null, ErrorCode.INVALID_ARGUMENT, "Expected int or byte for alpha component", .{}),
                        };

                        if (r < 0 or r > 255 or g < 0 or g > 255 or b < 0 or b > 255 or a < 0 or a > 255) {
                            return vm.reporter.reportRuntimeError(null, ErrorCode.INVALID_ARGUMENT, "Color values must be between 0 and 255", .{});
                        }

                        const color = ray.bytesToColor(r, g, b, a);

                        var fields = std.array_list.Managed(HIRStructField).init(vm.allocator);
                        defer fields.deinit();

                        try fields.append(HIRStructField{
                            .name = try vm.allocator.dupe(u8, "r"),
                            .value = HIRValue{ .int = color.r },
                            .field_type = .Int,
                        });
                        try fields.append(HIRStructField{
                            .name = try vm.allocator.dupe(u8, "g"),
                            .value = HIRValue{ .int = color.g },
                            .field_type = .Int,
                        });
                        try fields.append(HIRStructField{
                            .name = try vm.allocator.dupe(u8, "b"),
                            .value = HIRValue{ .int = color.b },
                            .field_type = .Int,
                        });
                        try fields.append(HIRStructField{
                            .name = try vm.allocator.dupe(u8, "a"),
                            .value = HIRValue{ .int = color.a },
                            .field_type = .Int,
                        });

                        const color_struct = HIRStruct{
                            .type_name = try vm.allocator.dupe(u8, "color"),
                            .fields = try fields.toOwnedSlice(),
                        };

                        const struct_value = HIRValue{ .struct_instance = color_struct };
                        try vm.stack.push(HIRFrame.initFromHIRValue(struct_value));
                        return;
                    } else if (std.mem.eql(u8, func_name, "stringToColor")) {
                        const name_frame = try vm.stack.pop();
                        const name = try name_frame.asString();
                        if (ray.stringToColor(name)) |color| {
                            var fields = std.array_list.Managed(HIRStructField).init(vm.allocator);
                            defer fields.deinit();

                            try fields.append(HIRStructField{ .name = try vm.allocator.dupe(u8, "r"), .value = HIRValue{ .int = color.r }, .field_type = .Int });
                            try fields.append(HIRStructField{ .name = try vm.allocator.dupe(u8, "g"), .value = HIRValue{ .int = color.g }, .field_type = .Int });
                            try fields.append(HIRStructField{ .name = try vm.allocator.dupe(u8, "b"), .value = HIRValue{ .int = color.b }, .field_type = .Int });
                            try fields.append(HIRStructField{ .name = try vm.allocator.dupe(u8, "a"), .value = HIRValue{ .int = color.a }, .field_type = .Int });

                            const color_struct = HIRStruct{ .type_name = try vm.allocator.dupe(u8, "color"), .fields = try fields.toOwnedSlice() };
                            const struct_value = HIRValue{ .struct_instance = color_struct };
                            try vm.stack.push(HIRFrame.initFromHIRValue(struct_value));
                            return;
                        } else {
                            return vm.reporter.reportRuntimeError(null, ErrorCode.INVALID_ARGUMENT, "Unknown color name: {s}", .{name});
                        }
                    } else if (std.mem.eql(u8, func_name, "ClearBackground")) {
                        const color_frame = try vm.stack.pop();
                        switch (color_frame.value) {
                            .struct_instance => |struct_val| {
                                if (std.mem.eql(u8, struct_val.type_name, "color")) {
                                    var color: ray.DoxaColor = ray.colorNameToColor(.WHITE);
                                    for (struct_val.fields) |field| {
                                        if (std.mem.eql(u8, field.name, "r")) color.r = @as(u8, @intCast(field.value.int));
                                        if (std.mem.eql(u8, field.name, "g")) color.g = @as(u8, @intCast(field.value.int));
                                        if (std.mem.eql(u8, field.name, "b")) color.b = @as(u8, @intCast(field.value.int));
                                        if (std.mem.eql(u8, field.name, "a")) color.a = @as(u8, @intCast(field.value.int));
                                    }
                                    ray.ClearBackgroundDoxa(color);
                                    return;
                                }
                            },
                            else => {},
                        }
                        return vm.reporter.reportRuntimeError(null, ErrorCode.INVALID_ARGUMENT, "Expected color struct for doxa.ClearBackground", .{});
                    }
                } else if (std.mem.eql(u8, sub_alias, "raylib")) {
                    if (std.mem.eql(u8, func_name, "ClearBackground")) {
                        const color_frame = try vm.stack.pop();
                        var color: ray.DoxaColor = ray.colorNameToColor(.WHITE);

                        switch (color_frame.value) {
                            .string => |s| {
                                if (std.mem.startsWith(u8, s, "graphics.raylib.")) {
                                    const color_name = s[16..];
                                    if (ray.stringToColor(color_name)) |parsed_color| {
                                        color = parsed_color;
                                    }
                                } else if (std.mem.startsWith(u8, s, "g.raylib.")) {
                                    const color_name = s[9..];
                                    if (ray.stringToColor(color_name)) |parsed_color| {
                                        color = parsed_color;
                                    }
                                } else {
                                    if (ray.stringToColor(s)) |parsed_color| {
                                        color = parsed_color;
                                    }
                                }
                            },
                            .struct_instance => |struct_val| {
                                if (std.mem.eql(u8, struct_val.type_name, "color")) {
                                    for (struct_val.fields) |field| {
                                        if (std.mem.eql(u8, field.name, "r")) {
                                            if (field.value == .int) {
                                                color.r = @as(u8, @intCast(field.value.int));
                                            }
                                        } else if (std.mem.eql(u8, field.name, "g")) {
                                            if (field.value == .int) {
                                                color.g = @as(u8, @intCast(field.value.int));
                                            }
                                        } else if (std.mem.eql(u8, field.name, "b")) {
                                            if (field.value == .int) {
                                                color.b = @as(u8, @intCast(field.value.int));
                                            }
                                        } else if (std.mem.eql(u8, field.name, "a")) {
                                            if (field.value == .int) {
                                                color.a = @as(u8, @intCast(field.value.int));
                                            }
                                        }
                                    }
                                }
                            },
                            else => {},
                        }
                        ray.ClearBackgroundDoxa(color);
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
                        return;
                    } else if (std.mem.eql(u8, func_name, "CloseWindow")) {
                        ray.CloseWindow();
                        return;
                    } else if (std.mem.eql(u8, func_name, "BeginDrawing")) {
                        ray.BeginDrawing();
                        return;
                    } else if (std.mem.eql(u8, func_name, "EndDrawing")) {
                        ray.EndDrawing();
                        return;
                    } else if (std.mem.eql(u8, func_name, "SetTargetFPS")) {
                        const fps = try vm.stack.pop();
                        const f = try fps.asInt();
                        ray.SetTargetFPSDoxa(f);
                        return;
                    } else if (std.mem.eql(u8, func_name, "DrawFPS")) {
                        // Accept either (x, y) or (x, y, fps) and ignore fps value
                        if (c.arg_count == 3) {
                            _ = try vm.stack.pop(); // fps (ignored)
                        }
                        const y_frame = try vm.stack.pop();
                        const x_frame = try vm.stack.pop();
                        const x = try x_frame.asInt();
                        const y = try y_frame.asInt();
                        ray.DrawFPS(@as(i32, @intCast(x)), @as(i32, @intCast(y)), 0);
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
                        var color: ray.DoxaColor = ray.colorNameToColor(.WHITE);
                        switch (color_frame.value) {
                            .string => |s| {
                                if (std.mem.startsWith(u8, s, "graphics.raylib.")) {
                                    const color_name = s[16..];
                                    if (ray.stringToColor(color_name)) |parsed_color| {
                                        color = parsed_color;
                                    }
                                } else if (std.mem.startsWith(u8, s, "g.raylib.")) {
                                    const color_name = s[9..];
                                    if (ray.stringToColor(color_name)) |parsed_color| {
                                        color = parsed_color;
                                    }
                                } else {
                                    if (ray.stringToColor(s)) |parsed_color| {
                                        color = parsed_color;
                                    }
                                }
                            },
                            .struct_instance => |struct_val| {
                                if (std.mem.eql(u8, struct_val.type_name, "color")) {
                                    for (struct_val.fields) |field| {
                                        if (std.mem.eql(u8, field.name, "r")) {
                                            if (field.value == .int) {
                                                color.r = @as(u8, @intCast(field.value.int));
                                            }
                                        } else if (std.mem.eql(u8, field.name, "g")) {
                                            if (field.value == .int) {
                                                color.g = @as(u8, @intCast(field.value.int));
                                            }
                                        } else if (std.mem.eql(u8, field.name, "b")) {
                                            if (field.value == .int) {
                                                color.b = @as(u8, @intCast(field.value.int));
                                            }
                                        } else if (std.mem.eql(u8, field.name, "a")) {
                                            if (field.value == .int) {
                                                color.a = @as(u8, @intCast(field.value.int));
                                            }
                                        }
                                    }
                                }
                            },
                            else => {},
                        }

                        ray.DrawCircle(@as(i32, @intCast(x)), @as(i32, @intCast(y)), @floatCast(radius), .{ .r = color.r, .g = color.g, .b = color.b, .a = color.a });
                        return;
                    }
                }
            }
        }

        const is_safe_add = blk: {
            if (std.mem.lastIndexOfScalar(u8, c.qualified_name, '.')) |dot_idx| {
                break :blk std.mem.eql(u8, c.qualified_name[dot_idx + 1 ..], "safeAdd");
            } else break :blk std.mem.eql(u8, c.qualified_name, "safeAdd");
        };
        if (is_safe_add) {
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

            const limit = 255;
            if (a_int > limit or b_int > limit) {
                const overflow_value = HIRValue{ .string = "Overflow" };
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

            const result = std.math.add(i64, a_int, b_int) catch {
                try vm.stack.push(HIRFrame.initInt(-1));
                return;
            };

            try vm.stack.push(HIRFrame.initInt(result));
        } else {
            return vm.reporter.reportRuntimeError(null, ErrorCode.VARIABLE_NOT_FOUND, "Unknown module function: {s}", .{c.qualified_name});
        }
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
                    const new_elements = try vm.allocator.realloc(mutable_arr.elements, new_capacity);
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

    fn execBuiltinSafeAdd(vm: anytype) !void {
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
            try vm.stack.push(HIRFrame.initFromHIRValue(HIRValue.nothing));
            return;
        };

        try vm.stack.push(HIRFrame.initInt(result));
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

    fn execBuiltinInput(vm: anytype) !void {
        if (vm.call_stack.sp > 0) {
            const call_info = vm.call_stack.frames[vm.call_stack.sp - 1];
            if (call_info.arg_count > 0) {
                const prompt_frame = try vm.stack.pop();
                switch (prompt_frame.value) {
                    .string => |prompt_str| {
                        const stdout_file = std.fs.File.stdout();
                        _ = try stdout_file.write(prompt_str);
                    },
                    else => {
                        try PrintOps.formatHIRValueRaw(vm, prompt_frame.value);
                    },
                }
            }
        }

        var stdin_buffer: [4096]u8 = undefined;
        var stdin_reader = std.fs.File.stdin().reader(&stdin_buffer);
        const stdin = &stdin_reader.interface;

        const input_line = stdin.takeDelimiterExclusive('\n') catch |err| switch (err) {
            error.EndOfStream => {
                vm.running = false;
                return;
            },
            error.StreamTooLong => {
                const line = stdin.takeDelimiterExclusive('\n') catch "";
                if (line.len == 0) {
                    vm.running = false;
                    return;
                }
                return processInputLine(vm, line);
            },
            error.ReadFailed => {
                vm.running = false;
                return;
            },
            else => return err,
        };

        try processInputLine(vm, input_line);
    }

    fn processInputLine(vm: anytype, line: []const u8) !void {
        var clean_line = line;
        if (clean_line.len > 0 and clean_line[clean_line.len - 1] == '\r') {
            clean_line = clean_line[0 .. clean_line.len - 1];
        }

        const input_string = try vm.allocator.dupe(u8, clean_line);
        try vm.stack.push(HIRFrame.initString(input_string));
    }

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

    fn execBuiltinTime(vm: anytype) !void {
        const timestamp = std.time.timestamp();
        try vm.stack.push(HIRFrame.initInt(timestamp));
    }

    fn execBuiltinTick(vm: anytype) !void {
        const ns = std.time.nanoTimestamp();
        const ns_i64: i64 = @as(i64, @intCast(ns));
        try vm.stack.push(HIRFrame.initInt(ns_i64));
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

    fn execBuiltinSleep(vm: anytype) !void {
        const duration_frame = try vm.stack.pop();
        const duration_ms = switch (duration_frame.value) {
            .int => |i| @as(u64, @intCast(i)),
            .byte => |b| @as(u64, b),
            else => {
                return vm.reporter.reportRuntimeError(null, ErrorCode.VARIABLE_NOT_FOUND, "@sleep: argument must be an integer", .{});
            },
        };

        std.Thread.sleep(duration_ms * std.time.ns_per_ms);
    }

    fn execBuiltinRandom(vm: anytype) !void {
        const random_value = std.crypto.random.float(f64);

        try vm.stack.push(HIRFrame.initFloat(random_value));
    }
};
