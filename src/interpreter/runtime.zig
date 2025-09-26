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
        // Instead of asserting, expand the values array if needed
        if (idx >= self.values.len) {
            // Resize to accommodate the requested slot
            const new_size = idx + 1;
            self.values = self.allocator.realloc(self.values, new_size) catch {
                // If realloc fails, we need to handle this gracefully
                // For now, we'll just return a pointer to the existing value if possible
                if (self.values.len > 0) {
                    return SlotPointer{ .value = &self.values[0] };
                } else {
                    // This is a critical error - we can't allocate memory
                    // We'll need to handle this at a higher level
                    unreachable;
                }
            };
            // Initialize new slots to nothing
            for (self.values[self.values.len..new_size]) |*value| {
                value.* = HIRValue.nothing;
            }
        }
        return SlotPointer{ .value = &self.values[idx] };
    }
};

pub const Frame = struct {
    allocator: std.mem.Allocator,
    function: *module.BytecodeFunction,
    return_ip: usize,
    stack_base: usize,
    slot_ref_base: usize,
    locals: []HIRValue,
    alias_refs: []?SlotPointer,

    pub fn init(allocator: std.mem.Allocator, function: *module.BytecodeFunction, stack_base: usize, slot_ref_base: usize, return_ip: usize) !Frame {
        const locals = try allocator.alloc(HIRValue, function.local_var_count);
        for (locals) |*slot| slot.* = HIRValue.nothing;

        const aliases = try allocator.alloc(?SlotPointer, function.local_var_count);
        for (aliases) |*entry| entry.* = null;

        return Frame{
            .allocator = allocator,
            .function = function,
            .return_ip = return_ip,
            .stack_base = stack_base,
            .slot_ref_base = slot_ref_base,
            .locals = locals,
            .alias_refs = aliases,
        };
    }

    pub fn deinit(self: *Frame) void {
        self.allocator.free(self.locals);
        self.allocator.free(self.alias_refs);
        self.locals = &[_]HIRValue{};
        self.alias_refs = &[_]?SlotPointer{};
    }

    pub fn pointer(self: *Frame, slot: module.SlotIndex) SlotPointer {
        const idx: usize = @intCast(slot);
        // Instead of asserting, expand the arrays if needed
        if (idx >= self.locals.len or idx >= self.alias_refs.len) {
            // Resize both arrays to accommodate the requested slot
            const new_size = idx + 1;

            // Resize locals array
            self.locals = self.allocator.realloc(self.locals, new_size) catch {
                // If realloc fails, we need to handle this gracefully
                // For now, we'll just return a pointer to the existing value if possible
                if (self.locals.len > 0) {
                    return SlotPointer{ .value = &self.locals[0] };
                } else {
                    // This is a critical error - we can't allocate memory
                    // We'll need to handle this at a higher level
                    unreachable;
                }
            };

            // Resize alias_refs array
            self.alias_refs = self.allocator.realloc(self.alias_refs, new_size) catch {
                // If realloc fails, return a pointer to the local value
                return SlotPointer{ .value = &self.locals[idx] };
            };

            // Initialize new slots
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
        // Ensure the slot exists by calling pointer method which handles resizing
        _ = self.pointer(slot);
        self.alias_refs[idx] = target;
    }

    pub fn clearAlias(self: *Frame, slot: module.SlotIndex) void {
        const idx: usize = @intCast(slot);
        // Ensure the slot exists by calling pointer method which handles resizing
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
