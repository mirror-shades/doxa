const std = @import("std");
const module = @import("../codegen/bytecode/module.zig");
const core = @import("./core.zig");
const Errors = @import("../utils/errors.zig");
const ErrorList = Errors.ErrorList;
const HIRValue = @import("../codegen/hir/soxa_values.zig").HIRValue;
const HIRFrame = core.HIRFrame;
const Raylib = @import("../runtime/raylib.zig");

const BytecodeType = module.BytecodeType;

pub const SlotPointer = struct {
    value: *HIRValue,

    pub fn load(self: SlotPointer) HIRValue {
        return self.value.*;
    }

    pub fn store(self: SlotPointer, new_value: HIRValue) void {
        self.value.* = new_value;
    }
};

pub const ModuleState = struct {
    allocator: std.mem.Allocator,
    values: []HIRValue,

    pub fn init(allocator: std.mem.Allocator, slot_count: usize) !ModuleState {
        const slots = try allocator.alloc(HIRValue, slot_count);
        for (slots) |*slot| slot.* = HIRValue.nothing;
        return ModuleState{
            .allocator = allocator,
            .values = slots,
        };
    }

    pub fn deinit(self: *ModuleState) void {
        self.allocator.free(self.values);
        self.values = &[_]HIRValue{};
    }

    pub fn pointer(self: *ModuleState, slot: module.SlotIndex) SlotPointer {
        const idx: usize = @intCast(slot);
        if (idx >= self.values.len) {
            const new_size = idx + 1;
            self.values = self.allocator.realloc(self.values, new_size) catch {
                if (self.values.len > 0) {
                    return SlotPointer{ .value = &self.values[0] };
                } else {
                    unreachable;
                }
            };
            for (self.values[self.values.len..new_size]) |*value| {
                value.* = HIRValue.nothing;
            }
        }
        return SlotPointer{ .value = &self.values[idx] };
    }
};

pub const Frame = struct {
    allocator: std.mem.Allocator,
    arena: std.heap.ArenaAllocator,
    function: *module.BytecodeFunction,
    return_ip: usize,
    stack_base: usize,
    slot_ref_base: usize,
    locals: []HIRValue,
    alias_refs: []?SlotPointer,

    pub fn init(allocator: std.mem.Allocator, function: *module.BytecodeFunction, stack_base: usize, slot_ref_base: usize, return_ip: usize) !Frame {
        var arena = std.heap.ArenaAllocator.init(allocator);
        const frame_alloc = arena.allocator();

        const locals = try frame_alloc.alloc(HIRValue, function.local_var_count);
        for (locals) |*slot| slot.* = HIRValue.nothing;

        const aliases = try frame_alloc.alloc(?SlotPointer, function.local_var_count);
        for (aliases) |*entry| entry.* = null;

        return Frame{
            .allocator = allocator,
            .arena = arena,
            .function = function,
            .return_ip = return_ip,
            .stack_base = stack_base,
            .slot_ref_base = slot_ref_base,
            .locals = locals,
            .alias_refs = aliases,
        };
    }

    pub fn deinit(self: *Frame) void {
        self.arena.deinit();
        self.locals = &[_]HIRValue{};
        self.alias_refs = &[_]?SlotPointer{};
    }

    pub fn pointer(self: *Frame, slot: module.SlotIndex) SlotPointer {
        const idx: usize = @intCast(slot);
        if (idx >= self.locals.len or idx >= self.alias_refs.len) {
            const new_size = idx + 1;

            self.locals = self.allocator.realloc(self.locals, new_size) catch {
                if (self.locals.len > 0) {
                    return SlotPointer{ .value = &self.locals[0] };
                } else {
                    unreachable;
                }
            };

            self.alias_refs = self.allocator.realloc(self.alias_refs, new_size) catch {
                return SlotPointer{ .value = &self.locals[idx] };
            };

            for (self.locals[self.locals.len..new_size]) |*value| {
                value.* = HIRValue.nothing;
            }
            for (self.alias_refs[self.alias_refs.len..new_size]) |*ref| {
                ref.* = null;
            }
        }

        if (self.alias_refs[idx]) |alias_ptr| {
            return alias_ptr;
        }
        return SlotPointer{ .value = &self.locals[idx] };
    }

    pub fn bindAlias(self: *Frame, slot: module.SlotIndex, target: SlotPointer) void {
        const idx: usize = @intCast(slot);
        _ = self.pointer(slot);
        self.alias_refs[idx] = target;
    }

    pub fn clearAlias(self: *Frame, slot: module.SlotIndex) void {
        const idx: usize = @intCast(slot);
        _ = self.pointer(slot);
        self.alias_refs[idx] = null;
    }

    pub fn getSlotRefBase(self: Frame) usize {
        return self.slot_ref_base;
    }

    pub fn getStackBase(self: Frame) usize {
        return self.stack_base;
    }

    pub fn getReturnIp(self: Frame) usize {
        return self.return_ip;
    }
};

pub const OperandStack = struct {
    allocator: std.mem.Allocator,
    values: []HIRFrame,
    sp: usize = 0,

    pub fn init(allocator: std.mem.Allocator, capacity: usize) !OperandStack {
        const buffer = try allocator.alloc(HIRFrame, capacity);
        for (buffer) |*frame| frame.* = HIRFrame.initNothing();
        return OperandStack{
            .allocator = allocator,
            .values = buffer,
            .sp = 0,
        };
    }

    pub fn deinit(self: *OperandStack) void {
        self.allocator.free(self.values);
        self.values = &[_]HIRFrame{};
        self.sp = 0;
    }

    pub fn reset(self: *OperandStack) void {
        self.sp = 0;
    }

    pub fn push(self: *OperandStack, frame: HIRFrame) ErrorList!void {
        if (self.sp >= self.values.len) return ErrorList.StackOverflow;
        self.values[self.sp] = frame;
        self.sp += 1;
    }

    pub fn pushValue(self: *OperandStack, value: HIRValue) ErrorList!void {
        return self.push(HIRFrame.initFromHIRValue(value));
    }

    pub fn pop(self: *OperandStack) ErrorList!HIRFrame {
        if (self.sp == 0) return ErrorList.StackUnderflow;
        self.sp -= 1;
        return self.values[self.sp];
    }

    pub fn popValue(self: *OperandStack) ErrorList!HIRValue {
        const frame = try self.pop();
        return frame.value;
    }

    pub fn peek(self: *OperandStack) ErrorList!HIRFrame {
        if (self.sp == 0) return ErrorList.StackUnderflow;
        return self.values[self.sp - 1];
    }

    pub fn peekValue(self: *OperandStack) ErrorList!HIRValue {
        const frame = try self.peek();
        return frame.value;
    }

    pub fn swapTop(self: *OperandStack) ErrorList!void {
        if (self.sp < 2) return ErrorList.StackUnderflow;
        const top = self.sp - 1;
        const prev = top - 1;
        const tmp = self.values[top];
        self.values[top] = self.values[prev];
        self.values[prev] = tmp;
    }

    pub fn len(self: OperandStack) usize {
        return self.sp;
    }

    pub fn size(self: OperandStack) i64 {
        return @intCast(self.sp);
    }

    pub fn truncate(self: *OperandStack, new_len: usize) void {
        if (new_len <= self.sp) self.sp = new_len;
    }
};

pub const SlotResolution = struct {
    pointer: SlotPointer,
    type_tag: BytecodeType,
};

const HIRStructField = @import("../codegen/hir/soxa_values.zig").HIRStructField;

pub const GraphicsRuntime = struct {
    allocator: std.mem.Allocator,
    reporter: *@import("../utils/reporting.zig").Reporter,

    pub fn init(allocator: std.mem.Allocator, reporter: *@import("../utils/reporting.zig").Reporter) GraphicsRuntime {
        return GraphicsRuntime{
            .allocator = allocator,
            .reporter = reporter,
        };
    }

    pub fn handleGraphicsCall(self: *GraphicsRuntime, module_alias: []const u8, sub_alias: []const u8, func_name: []const u8, stack: *OperandStack, scope_stack: *std.array_list.Managed(@import("./vm.zig").ScopeRecord)) !bool {
        const is_graphics = std.mem.eql(u8, module_alias, "graphics") or std.mem.eql(u8, module_alias, "g");
        if (!is_graphics) return false;

        if (std.mem.eql(u8, sub_alias, "doxa")) {
            return try self.handleDoxaCall(func_name, stack, scope_stack);
        }

        if (std.mem.eql(u8, sub_alias, "raylib")) {
            return try self.handleRaylibCall(func_name, stack, scope_stack);
        }

        return false;
    }

    fn handleDoxaCall(self: *GraphicsRuntime, func_name: []const u8, stack: *OperandStack, scope_stack: *std.array_list.Managed(@import("./vm.zig").ScopeRecord)) !bool {
        if (std.mem.eql(u8, func_name, "Init")) {
            const name_frame = try stack.pop();
            const fps_frame = try stack.pop();
            const height_frame = try stack.pop();
            const width_frame = try stack.pop();

            const w = try width_frame.asInt();
            const h = try height_frame.asInt();
            const fps = try fps_frame.asInt();
            const name = try name_frame.asString();

            try Raylib.InitWindowDoxa(w, h, name);
            Raylib.SetTargetFPSDoxa(fps);

            if (scope_stack.items.len > 0) {
                var scope = &scope_stack.items[scope_stack.items.len - 1];
                try scope.defer_actions.append(&Raylib.CloseWindow);
            }

            return true;
        } else if (std.mem.eql(u8, func_name, "Draw")) {
            Raylib.BeginDrawing();

            if (scope_stack.items.len > 0) {
                var scope = &scope_stack.items[scope_stack.items.len - 1];
                try scope.defer_actions.append(&Raylib.EndDrawing);
            }

            return true;
        } else if (std.mem.eql(u8, func_name, "Running")) {
            const should_close = Raylib.WindowShouldClose();
            try stack.pushValue(HIRValue{ .tetra = if (should_close) 0 else 1 });
            return true;
        } else if (std.mem.eql(u8, func_name, "bytesToColor")) {
            const a_frame = try stack.pop();
            const b_frame = try stack.pop();
            const g_frame = try stack.pop();
            const r_frame = try stack.pop();

            const r = switch (r_frame.value) {
                .byte => |byte_val| byte_val,
                .int => |i| if (i >= 0 and i <= 255) @as(u8, @intCast(i)) else 0,
                else => 0,
            };
            const g = switch (g_frame.value) {
                .byte => |byte_val| byte_val,
                .int => |i| if (i >= 0 and i <= 255) @as(u8, @intCast(i)) else 0,
                else => 0,
            };
            const b = switch (b_frame.value) {
                .byte => |byte_val| byte_val,
                .int => |i| if (i >= 0 and i <= 255) @as(u8, @intCast(i)) else 0,
                else => 0,
            };
            const a = switch (a_frame.value) {
                .byte => |byte_val| byte_val,
                .int => |i| if (i >= 0 and i <= 255) @as(u8, @intCast(i)) else 0,
                else => 0,
            };

            const color = Raylib.bytesToColor(r, g, b, a);
            const fields = try self.allocator.alloc(HIRStructField, 4);
            fields[0] = .{ .name = "r", .value = HIRValue{ .byte = color.r }, .field_type = .Byte };
            fields[1] = .{ .name = "g", .value = HIRValue{ .byte = color.g }, .field_type = .Byte };
            fields[2] = .{ .name = "b", .value = HIRValue{ .byte = color.b }, .field_type = .Byte };
            fields[3] = .{ .name = "a", .value = HIRValue{ .byte = color.a }, .field_type = .Byte };

            try stack.pushValue(HIRValue{ .struct_instance = .{
                .type_name = "Color",
                .fields = fields,
                .field_name = null,
                .path = null,
            } });
            return true;
        }

        return false;
    }

    fn handleRaylibCall(self: *GraphicsRuntime, func_name: []const u8, stack: *OperandStack, scope_stack: *std.array_list.Managed(@import("./vm.zig").ScopeRecord)) !bool {
        _ = scope_stack;
        if (std.mem.eql(u8, func_name, "ClearBackground")) {
            const color_frame = try stack.pop();
            try self.handleClearBackground(color_frame);
            return true;
        } else if (std.mem.eql(u8, func_name, "DrawCircle")) {
            const color_frame = try stack.pop();
            const radius_frame = try stack.pop();
            const y_frame = try stack.pop();
            const x_frame = try stack.pop();

            const x = try x_frame.asInt();
            const y = try y_frame.asInt();
            const radius = switch (radius_frame.value) {
                .int => |i| @as(f32, @floatFromInt(i)),
                .float => |f| @as(f32, @floatCast(f)),
                .byte => |b| @as(f32, @floatFromInt(b)),
                else => 0.0,
            };

            var r: u8 = 0;
            var g: u8 = 0;
            var b: u8 = 0;
            var a: u8 = 255;

            switch (color_frame.value) {
                .struct_instance => |s| {
                    for (s.fields) |field| {
                        if (std.mem.eql(u8, field.name, "r")) {
                            r = switch (field.value) {
                                .byte => |byte_val| byte_val,
                                .int => |i| if (i >= 0 and i <= 255) @as(u8, @intCast(i)) else 0,
                                else => 0,
                            };
                        } else if (std.mem.eql(u8, field.name, "g")) {
                            g = switch (field.value) {
                                .byte => |byte_val| byte_val,
                                .int => |i| if (i >= 0 and i <= 255) @as(u8, @intCast(i)) else 0,
                                else => 0,
                            };
                        } else if (std.mem.eql(u8, field.name, "b")) {
                            b = switch (field.value) {
                                .byte => |byte_val| byte_val,
                                .int => |i| if (i >= 0 and i <= 255) @as(u8, @intCast(i)) else 0,
                                else => 0,
                            };
                        } else if (std.mem.eql(u8, field.name, "a")) {
                            a = switch (field.value) {
                                .byte => |byte_val| byte_val,
                                .int => |i| if (i >= 0 and i <= 255) @as(u8, @intCast(i)) else 0,
                                else => 255,
                            };
                        }
                    }
                },
                else => {},
            }

            Raylib.DrawCircle(@intCast(x), @intCast(y), radius, .{ .r = r, .g = g, .b = b, .a = a });
            return true;
        } else if (std.mem.eql(u8, func_name, "InitWindow")) {
            const title_frame = try stack.pop();
            const height_frame = try stack.pop();
            const width_frame = try stack.pop();

            const width = try width_frame.asInt();
            const height = try height_frame.asInt();
            const title = try title_frame.asString();

            try Raylib.InitWindowDoxa(width, height, title);
            return true;
        } else if (std.mem.eql(u8, func_name, "BeginDrawing")) {
            Raylib.BeginDrawing();
            return true;
        } else if (std.mem.eql(u8, func_name, "EndDrawing")) {
            Raylib.EndDrawing();
            return true;
        } else if (std.mem.eql(u8, func_name, "WindowShouldClose")) {
            const should_close = Raylib.WindowShouldClose();
            try stack.pushValue(HIRValue{ .tetra = if (should_close) 1 else 0 });
            return true;
        } else if (std.mem.eql(u8, func_name, "CloseWindow")) {
            Raylib.CloseWindow();
            return true;
        }

        return false;
    }

    fn handleClearBackground(self: *GraphicsRuntime, color_frame: HIRFrame) !void {
        _ = self;
        switch (color_frame.value) {
            .struct_instance => |s| {
                var r: u8 = 0;
                var g: u8 = 0;
                var b: u8 = 0;
                var a: u8 = 255;

                for (s.fields) |field| {
                    if (std.mem.eql(u8, field.name, "r")) {
                        r = switch (field.value) {
                            .byte => |byte_val| byte_val,
                            .int => |i| if (i >= 0 and i <= 255) @as(u8, @intCast(i)) else 0,
                            else => 0,
                        };
                    } else if (std.mem.eql(u8, field.name, "g")) {
                        g = switch (field.value) {
                            .byte => |byte_val| byte_val,
                            .int => |i| if (i >= 0 and i <= 255) @as(u8, @intCast(i)) else 0,
                            else => 0,
                        };
                    } else if (std.mem.eql(u8, field.name, "b")) {
                        b = switch (field.value) {
                            .byte => |byte_val| byte_val,
                            .int => |i| if (i >= 0 and i <= 255) @as(u8, @intCast(i)) else 0,
                            else => 0,
                        };
                    } else if (std.mem.eql(u8, field.name, "a")) {
                        a = switch (field.value) {
                            .byte => |byte_val| byte_val,
                            .int => |i| if (i >= 0 and i <= 255) @as(u8, @intCast(i)) else 0,
                            else => 255,
                        };
                    }
                }

                Raylib.ClearBackgroundDoxa(.{ .r = r, .g = g, .b = b, .a = a });
            },
            .string => |color_str| {
                var color: ?Raylib.Color = null;

                if (std.mem.eql(u8, color_str, "SKYBLUE") or std.mem.eql(u8, color_str, "graphics.raylib.SKYBLUE")) {
                    color = Raylib.SKYBLUE;
                } else if (std.mem.eql(u8, color_str, "RED") or std.mem.eql(u8, color_str, "graphics.raylib.RED")) {
                    color = Raylib.RED;
                } else if (std.mem.eql(u8, color_str, "GREEN") or std.mem.eql(u8, color_str, "graphics.raylib.GREEN")) {
                    color = Raylib.GREEN;
                } else if (std.mem.eql(u8, color_str, "BLUE") or std.mem.eql(u8, color_str, "graphics.raylib.BLUE")) {
                    color = Raylib.BLUE;
                } else if (std.mem.eql(u8, color_str, "YELLOW") or std.mem.eql(u8, color_str, "graphics.raylib.YELLOW")) {
                    color = Raylib.YELLOW;
                } else if (std.mem.eql(u8, color_str, "PURPLE") or std.mem.eql(u8, color_str, "graphics.raylib.PURPLE")) {
                    color = Raylib.PURPLE;
                } else if (std.mem.eql(u8, color_str, "PINK") or std.mem.eql(u8, color_str, "graphics.raylib.PINK")) {
                    color = Raylib.PINK;
                } else if (std.mem.eql(u8, color_str, "ORANGE") or std.mem.eql(u8, color_str, "graphics.raylib.ORANGE")) {
                    color = Raylib.ORANGE;
                } else if (std.mem.eql(u8, color_str, "WHITE") or std.mem.eql(u8, color_str, "graphics.raylib.WHITE")) {
                    color = Raylib.WHITE;
                } else if (std.mem.eql(u8, color_str, "BLACK") or std.mem.eql(u8, color_str, "graphics.raylib.BLACK")) {
                    color = Raylib.BLACK;
                } else if (std.mem.eql(u8, color_str, "GRAY") or std.mem.eql(u8, color_str, "graphics.raylib.GRAY")) {
                    color = Raylib.GRAY;
                } else if (std.mem.eql(u8, color_str, "LIGHTGRAY") or std.mem.eql(u8, color_str, "graphics.raylib.LIGHTGRAY")) {
                    color = Raylib.LIGHTGRAY;
                } else if (std.mem.eql(u8, color_str, "DARKGRAY") or std.mem.eql(u8, color_str, "graphics.raylib.DARKGRAY")) {
                    color = Raylib.DARKGRAY;
                } else if (std.mem.eql(u8, color_str, "GOLD") or std.mem.eql(u8, color_str, "graphics.raylib.GOLD")) {
                    color = Raylib.GOLD;
                } else if (std.mem.eql(u8, color_str, "LIME") or std.mem.eql(u8, color_str, "graphics.raylib.LIME")) {
                    color = Raylib.LIME;
                } else if (std.mem.eql(u8, color_str, "VIOLET") or std.mem.eql(u8, color_str, "graphics.raylib.VIOLET")) {
                    color = Raylib.VIOLET;
                } else if (std.mem.eql(u8, color_str, "BROWN") or std.mem.eql(u8, color_str, "graphics.raylib.BROWN")) {
                    color = Raylib.BROWN;
                } else if (std.mem.eql(u8, color_str, "BEIGE") or std.mem.eql(u8, color_str, "graphics.raylib.BEIGE")) {
                    color = Raylib.BEIGE;
                }

                if (color) |c| {
                    Raylib.ClearBackgroundDoxa(.{ .r = c.r, .g = c.g, .b = c.b, .a = c.a });
                } else {
                    Raylib.ClearBackgroundDoxa(.{ .r = 0, .g = 0, .b = 0, .a = 255 });
                }
            },
            else => {
                Raylib.ClearBackgroundDoxa(.{ .r = 0, .g = 0, .b = 0, .a = 255 });
            },
        }
    }

    pub fn handleGraphicsFieldAccess(self: *GraphicsRuntime, frame: HIRFrame, field_name: []const u8, stack: *OperandStack) !bool {
        if (frame.value == .string and std.mem.startsWith(u8, frame.value.string, "graphics.")) {
            if (std.mem.eql(u8, frame.value.string, "graphics.raylib")) {
                if (std.mem.eql(u8, field_name, "SKYBLUE")) {
                    const skyblue_color = Raylib.SKYBLUE;
                    const fields = try self.allocator.alloc(HIRStructField, 4);
                    fields[0] = .{ .name = "r", .value = HIRValue{ .byte = skyblue_color.r }, .field_type = .Byte };
                    fields[1] = .{ .name = "g", .value = HIRValue{ .byte = skyblue_color.g }, .field_type = .Byte };
                    fields[2] = .{ .name = "b", .value = HIRValue{ .byte = skyblue_color.b }, .field_type = .Byte };
                    fields[3] = .{ .name = "a", .value = HIRValue{ .byte = skyblue_color.a }, .field_type = .Byte };

                    try stack.push(HIRFrame.initFromHIRValue(HIRValue{ .struct_instance = .{
                        .type_name = "Color",
                        .fields = fields,
                        .field_name = null,
                        .path = null,
                    } }));
                    return true;
                }
            }
            const full_name = try std.fmt.allocPrint(self.allocator, "{s}.{s}", .{ frame.value.string, field_name });
            try stack.push(HIRFrame.initFromHIRValue(HIRValue{ .string = full_name }));
            return true;
        }
        return false;
    }
};
