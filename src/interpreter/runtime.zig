const std = @import("std");
const module = @import("../codegen/bytecode/module.zig");
const core = @import("./core.zig");
const Errors = @import("../utils/errors.zig");
const ErrorList = Errors.ErrorList;
const HIRValue = @import("../codegen/hir/soxa_values.zig").HIRValue;
const HIRFrame = core.HIRFrame;

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
    initialized: []bool,

    pub fn init(allocator: std.mem.Allocator, slot_count: usize) !ModuleState {
        const slots = try allocator.alloc(HIRValue, slot_count);
        for (slots) |*slot| slot.* = HIRValue.nothing;
        const flags = try allocator.alloc(bool, slot_count);
        for (flags) |*flag| flag.* = false;
        return ModuleState{
            .allocator = allocator,
            .values = slots,
            .initialized = flags,
        };
    }

    pub fn deinit(self: *ModuleState) void {
        self.allocator.free(self.values);
        self.values = &[_]HIRValue{};
        self.allocator.free(self.initialized);
        self.initialized = &[_]bool{};
    }

    pub fn pointer(self: *ModuleState, slot: module.SlotIndex) SlotPointer {
        const idx: usize = @intCast(slot);
        if (idx >= self.values.len) {
            const new_size = idx + 1;
            const old_len = self.values.len;
            self.values = self.allocator.realloc(self.values, new_size) catch {
                if (self.values.len > 0) {
                    return SlotPointer{ .value = &self.values[0] };
                } else {
                    unreachable;
                }
            };
            self.initialized = self.allocator.realloc(self.initialized, new_size) catch {
                if (self.initialized.len > 0) {
                    return SlotPointer{ .value = &self.values[0] };
                } else {
                    unreachable;
                }
            };
            for (self.values[old_len..new_size]) |*value| {
                value.* = HIRValue.nothing;
            }
            for (self.initialized[old_len..new_size]) |*flag| {
                flag.* = false;
            }
        }
        return SlotPointer{ .value = &self.values[idx] };
    }

    pub fn hasValue(self: *ModuleState, slot: module.SlotIndex) bool {
        const idx: usize = @intCast(slot);
        if (idx >= self.initialized.len) return false;
        return self.initialized[idx];
    }

    pub fn markInitialized(self: *ModuleState, slot: module.SlotIndex) void {
        const idx: usize = @intCast(slot);
        if (idx >= self.initialized.len) return;
        self.initialized[idx] = true;
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
            const arena_alloc = self.arena.allocator();

            self.locals = arena_alloc.realloc(self.locals, new_size) catch {
                if (self.locals.len > 0) {
                    return SlotPointer{ .value = &self.locals[0] };
                } else {
                    unreachable;
                }
            };

            self.alias_refs = arena_alloc.realloc(self.alias_refs, new_size) catch {
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
        if (self.sp >= self.values.len) {
            // Dynamic stack growth - double capacity
            const new_capacity = self.values.len * 2;
            const new_values = self.allocator.realloc(self.values, new_capacity) catch {
                return ErrorList.StackOverflow;
            };
            self.values = new_values;
        }
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
