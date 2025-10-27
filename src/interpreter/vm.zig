const std = @import("std");
const module = @import("../codegen/bytecode/module.zig");
const hir_types = @import("../codegen/hir/soxa_types.zig");
const hir_instructions = @import("../codegen/hir/soxa_instructions.zig");
const Reporting = @import("../utils/reporting.zig");
const Location = Reporting.Location;
const Reporter = Reporting.Reporter;
const hir_values = @import("../codegen/hir/soxa_values.zig");
const HIRValue = hir_values.HIRValue;
const HIRStructField = hir_values.HIRStructField;
const HIRMapEntry = hir_values.HIRMapEntry;
const HIRMap = hir_values.HIRMap;
const HIRStruct = hir_values.HIRStruct;
const core = @import("core.zig");
const HIRFrame = core.HIRFrame;
const runtime = @import("runtime.zig");
const MemoryManager = @import("../utils/memory.zig").MemoryManager;
const memory = @import("../utils/memory.zig");
const StringInterner = memory.StringInterner;
const CustomTypeInfo = memory.CustomTypeInfo;
const Managed = std.array_list.Managed;
const ops_arith = @import("ops/arith.zig");
const ops_compare = @import("ops/compare.zig");
const ops_logical = @import("ops/logical.zig");
const ops_strings = @import("ops/strings.zig");
const ops_array = @import("ops/array.zig");
const ops_type = @import("ops/type.zig");
const PrintOps = @import("../runtime/print.zig").PrintOps;
const Errors = @import("../utils/errors.zig");
const ErrorList = Errors.ErrorList;
const ErrorCode = Errors.ErrorCode;

const DeferAction = *const fn () void;
const DEFAULT_STACK_CAPACITY: usize = 1024 * 1024; // 1M frames initial capacity, grows dynamically

pub const ScopeRecord = struct {
    id: u32,
    var_count: u32,
    alias_slots: Managed(module.SlotIndex),
    catch_target: ?usize = null,
    defer_actions: Managed(DeferAction),
    arena: std.heap.ArenaAllocator,

    fn init(allocator: std.mem.Allocator, id: u32, var_count: u32) ScopeRecord {
        return ScopeRecord{
            .id = id,
            .var_count = var_count,
            .alias_slots = Managed(module.SlotIndex).init(allocator),
            .catch_target = null,
            .defer_actions = Managed(DeferAction).init(allocator),
            .arena = std.heap.ArenaAllocator.init(allocator),
        };
    }

    fn deinit(self: *ScopeRecord) void {
        self.alias_slots.deinit();
        self.defer_actions.deinit();
        self.arena.deinit();
    }

    fn recordAlias(self: *ScopeRecord, slot: module.SlotIndex) !void {
        try self.alias_slots.append(slot);
    }
};

const VmError = error{
    UnimplementedInstruction,
    NoActiveFrame,
    MissingModule,
    InvalidAliasReference,
    OutOfMemory,
} || ErrorList;

pub const VM = struct {
    bytecode: *module.BytecodeModule,
    reporter: *Reporter,
    memory_manager: *MemoryManager,
    allocator: std.mem.Allocator,
    stack: runtime.OperandStack,
    module_state: []runtime.ModuleState,
    frames: Managed(runtime.Frame),
    label_cache: []usize,
    slot_refs: Managed(runtime.SlotPointer),
    ip: usize = 0,
    running: bool = false,
    skip_increment: bool = false,
    string_interner: *StringInterner,
    custom_type_registry: std.StringHashMap(CustomTypeInfo),
    scope_stack: Managed(ScopeRecord),
    try_stack: Managed(usize),
    skip_next_enter_scope: bool = false,

    pub fn init(allocator: std.mem.Allocator, bytecode: *module.BytecodeModule, reporter: *Reporter, memory_manager: *MemoryManager) !VM {
        const frame_list = Managed(runtime.Frame).init(allocator);

        const stack = try runtime.OperandStack.init(allocator, DEFAULT_STACK_CAPACITY);

        const module_count = bytecode.modules.len;
        const modules = try allocator.alloc(runtime.ModuleState, module_count);
        for (modules, 0..) |*state, idx| {
            const descriptor = bytecode.modules[idx];
            state.* = try runtime.ModuleState.init(allocator, descriptor.global_var_count);
        }

        var max_label_id: usize = 0;
        for (bytecode.instructions) |inst| {
            if (inst == .Label) {
                if (inst.Label.id > max_label_id) max_label_id = inst.Label.id;
            }
        }

        const label_cache = try allocator.alloc(usize, max_label_id + 1);
        for (label_cache) |*entry| entry.* = 0;

        const string_interner = try allocator.create(StringInterner);
        string_interner.* = StringInterner.init(allocator);

        var vm = VM{
            .bytecode = bytecode,
            .reporter = reporter,
            .memory_manager = memory_manager,
            .allocator = allocator,
            .stack = stack,
            .module_state = modules,
            .frames = frame_list,
            .label_cache = label_cache,
            .slot_refs = Managed(runtime.SlotPointer).init(allocator),
            .ip = 0,
            .running = false,
            .string_interner = string_interner,
            .custom_type_registry = std.StringHashMap(CustomTypeInfo).init(allocator),
            .scope_stack = Managed(ScopeRecord).init(allocator),
            .try_stack = Managed(usize).init(allocator),
            .skip_next_enter_scope = false,
        };

        vm.indexLabels();

        // Register the Token enum for proper enum member access
        try vm.registerTokenEnum();

        // Initialize the Token variable in module state
        try vm.initializeTokenVariable();

        return vm;
    }

    pub fn deinit(self: *VM) void {
        while (self.frames.pop()) |frame_val| {
            var frame = frame_val;
            frame.deinit();
        }
        self.frames.deinit();

        self.clearScopeStack();
        self.scope_stack.deinit();
        self.try_stack.deinit();

        self.custom_type_registry.deinit();

        self.slot_refs.deinit();
        for (self.module_state) |*state| {
            state.deinit();
        }
        self.allocator.free(self.module_state);
        self.allocator.free(self.label_cache);
        self.stack.deinit();
        self.string_interner.deinit();
        self.allocator.destroy(self.string_interner);
    }

    pub fn reset(self: *VM) void {
        self.ip = 0;
        self.running = false;
        self.stack.reset();
        self.slot_refs.clearRetainingCapacity();
        self.skip_increment = false;
        while (self.frames.pop()) |frame_val| {
            var frame = frame_val;
            frame.deinit();
        }
        self.clearScopeStack();
        self.scope_stack.clearRetainingCapacity();
        self.try_stack.clearRetainingCapacity();
        self.skip_next_enter_scope = false;
    }

    pub fn run(self: *VM) !void {
        try self.prepareEntryFrame();
        self.running = true;

        while (self.running and self.ip < self.bytecode.instructions.len) {
            const inst = self.bytecode.instructions[self.ip];
            self.skip_increment = false;
            try self.execute(inst);
            if (!self.skip_increment) {
                self.ip += 1;
            }
        }
    }

    fn pushValue(self: *VM, value: HIRValue) VmError!void {
        try self.stack.pushValue(value);
    }

    fn popFrame(self: *VM) VmError!HIRFrame {
        return try self.stack.pop();
    }

    fn popValue(self: *VM) VmError!HIRValue {
        if (self.stack.sp == 0) {
            std.debug.print("StackUnderflow: Attempting to pop from empty stack at IP {d}\n", .{self.ip});
            std.debug.print("Current instruction: {any}\n", .{self.bytecode.instructions[self.ip]});
            return ErrorList.StackUnderflow;
        }
        return try self.stack.popValue();
    }
    pub fn execute(self: *VM, inst: module.Instruction) VmError!void {
        switch (inst) {
            .PushConst => |payload| {
                const value = self.bytecode.constants[payload.constant_index];
                try self.stack.pushValue(value);
            },
            .Arith => |payload| {
                try ops_arith.exec(self, .{
                    .op = payload.op,
                    .operand_type = toHIRType(payload.type_tag),
                });
            },
            .Convert => |payload| {
                try ops_type.TypeOps.execConvert(self, .{
                    .from_type = toHIRType(payload.from),
                    .to_type = toHIRType(payload.to),
                });
            },
            .Compare => |payload| {
                try ops_compare.exec(self, .{
                    .op = payload.op,
                    .operand_type = toHIRType(payload.type_tag),
                });
            },
            .LogicalOp => |payload| {
                try ops_logical.exec(self, payload);
            },
            .StringOp => |payload| {
                try ops_strings.exec(self, payload);
            },
            .TypeCheck => |payload| {
                try ops_type.TypeOps.execTypeCheck(self, .{ .target_type = payload.type_name });
            },
            .TypeOf => |payload| {
                try ops_type.TypeOps.execTypeOf(self, .{ .value_type = toHIRType(payload.value_type) });
            },
            .Exists => |payload| {
                _ = payload;
                try self.execQuantifier(true);
            },
            .Forall => |payload| {
                _ = payload;
                try self.execQuantifier(false);
            },
            .StructNew => |payload| {
                try self.execStructNew(payload);
            },
            .EnumNew => |payload| {
                try self.execEnumNew(payload);
            },
            .StoreFieldName => |payload| {
                try self.execStoreFieldName(payload);
            },
            .GetField => |payload| {
                try self.execGetField(payload);
            },
            .SetField => |payload| {
                try self.execSetField(payload);
            },
            .LoadSlot => |operand| {
                const ptr = try self.resolveSlot(operand);
                const value = ptr.load();
                try self.stack.pushValue(value);
            },
            .StoreSlot => |payload| {
                var value = try self.popValue();
                const ptr = try self.resolveSlot(payload.target);
                // Promote value lifetime based on destination
                switch (payload.target.kind) {
                    .local => {
                        const alloc = self.frameAllocator();
                        value = try self.deepCopyValueToAllocator(alloc, value);
                    },
                    .module_global, .imported_module => {
                        const module_id = payload.target.module_id orelse return error.MissingModule;
                        const state = try self.resolveModuleState(module_id);
                        value = try self.deepCopyValueToAllocator(state.allocator, value);
                    },
                    .alias, .builtin => {},
                }
                ptr.store(value);
            },
            .StoreConstSlot => |operand| {
                const value = try self.popValue();
                const ptr = try self.resolveSlot(operand);
                ptr.store(value);
            },
            .PushStorageRef => |operand| {
                const ptr = try self.resolveSlot(operand);
                const ref_id = try self.cacheSlotPointer(ptr);
                try self.stack.pushValue(HIRValue{ .storage_id_ref = ref_id });
            },
            .BindAlias => |payload| {
                const ref_value = try self.popValue();
                switch (ref_value) {
                    .storage_id_ref => |id| {
                        const frame = try self.currentFrame();
                        const ptr = try self.slotRefFromId(id);
                        frame.bindAlias(payload.alias_slot, ptr);
                        try self.trackAlias(payload.alias_slot);
                    },
                    else => return error.InvalidAliasReference,
                }
            },
            .LoadAlias => |payload| {
                const frame = try self.currentFrame();
                const ptr = frame.pointer(payload.slot_index);
                const value = ptr.load();
                try self.stack.pushValue(value);
            },
            .StoreAlias => |payload| {
                const value = try self.popValue();
                const frame = try self.currentFrame();
                const ptr = frame.pointer(payload.slot_index);
                ptr.store(value);
            },
            .ResolveAlias => |payload| {
                const frame = try self.currentFrame();
                const ptr = frame.pointer(payload.target_slot);
                const value = ptr.load();
                try self.stack.pushValue(value);
            },
            .Dup => {
                if (self.stack.sp == 0) {
                    self.reporter.reportRuntimeError(null, ErrorCode.STACK_UNDERFLOW, "Attempting to dup from empty stack", .{});
                    return ErrorList.StackUnderflow;
                }
                const value = try self.stack.peek();
                try self.stack.push(HIRFrame.initFromHIRValue(value.value));
            },
            .Pop => {
                _ = try self.stack.pop();
            },
            .Swap => {
                const top = try self.stack.pop();
                const second = try self.stack.pop();
                try self.stack.push(top);
                try self.stack.push(second);
            },
            .ArrayNew => |payload| {
                try ops_array.exec(self, .{ .ArrayNew = .{
                    .element_type = toHIRTypeWithNested(self, payload.element_type, payload.nested_element_type),
                    .size = payload.static_size,
                    .nested_element_type = toOptionalHIRType(payload.nested_element_type),
                } });
            },
            .ArrayGet => |payload| {
                try ops_array.exec(self, .{ .ArrayGet = .{
                    .bounds_check = payload.bounds_check,
                } });
            },
            .ArraySet => |payload| {
                try ops_array.exec(self, .{ .ArraySet = .{
                    .bounds_check = payload.bounds_check,
                } });
            },
            .ArrayPush => |payload| {
                try ops_array.exec(self, .{ .ArrayPush = .{
                    .resize_behavior = payload.resize,
                } });
            },
            .ArrayPop => {
                try ops_array.exec(self, .{ .ArrayPop = {} });
            },
            .ArrayInsert => {
                try ops_array.exec(self, .{ .ArrayInsert = {} });
            },
            .ArrayRemove => {
                try ops_array.exec(self, .{ .ArrayRemove = {} });
            },
            .ArraySlice => {
                try ops_array.exec(self, .{ .ArraySlice = {} });
            },
            .ArrayLen => {
                try ops_array.exec(self, .{ .ArrayLen = {} });
            },
            .ArrayConcat => {
                try ops_array.exec(self, .{ .ArrayConcat = {} });
            },
            .Range => |payload| {
                try ops_array.exec(self, .{ .Range = .{
                    .element_type = toHIRType(payload.element_type),
                } });
            },
            .ArrayGetAndAdd => |payload| {
                try ops_array.exec(self, .{ .ArrayGetAndAdd = .{
                    .bounds_check = payload.bounds_check,
                } });
            },
            .ArrayGetAndSub => |payload| {
                try ops_array.exec(self, .{ .ArrayGetAndSub = .{
                    .bounds_check = payload.bounds_check,
                } });
            },
            .ArrayGetAndMul => |payload| {
                try ops_array.exec(self, .{ .ArrayGetAndMul = .{
                    .bounds_check = payload.bounds_check,
                } });
            },
            .ArrayGetAndDiv => |payload| {
                try ops_array.exec(self, .{ .ArrayGetAndDiv = .{
                    .bounds_check = payload.bounds_check,
                } });
            },
            .ArrayGetAndMod => |payload| {
                try ops_array.exec(self, .{ .ArrayGetAndMod = .{
                    .bounds_check = payload.bounds_check,
                } });
            },
            .ArrayGetAndPow => |payload| {
                try ops_array.exec(self, .{ .ArrayGetAndPow = .{
                    .bounds_check = payload.bounds_check,
                } });
            },
            .Map => |payload| {
                try self.execMap(payload);
            },
            .MapGet => {
                try self.execMapGet();
            },
            .MapSet => |payload| {
                try self.execMapSet(payload);
            },
            .Jump => |payload| {
                const target = self.label_cache[payload.label_id];
                self.jumpTo(target);
            },
            .JumpIfFalse => |payload| {
                const condition = try self.popValue();
                const truthy = try self.isTruthy(condition);
                if (!truthy) {
                    const target = self.label_cache[payload.label_id];
                    self.jumpTo(target);
                }
            },
            .JumpIfTrue => |payload| {
                const condition = try self.popValue();
                const truthy = try self.isTruthy(condition);
                if (truthy) {
                    const target = self.label_cache[payload.label_id];
                    self.jumpTo(target);
                }
            },
            .Call => |payload| {
                try self.handleCall(payload);
            },
            .TailCall => |payload| {
                try self.execTailCall(payload);
            },
            .Return => |payload| {
                try self.handleReturn(payload);
            },
            .Print => {
                try PrintOps.execPrint(self);
            },
            .PrintBegin => {
                // no-op for now (future: buffering)
            },
            .PrintStr => |payload| {
                if (payload.const_id >= self.bytecode.constants.len) return error.UnimplementedInstruction;
                const val = self.bytecode.constants[payload.const_id];
                switch (val) {
                    .string => |s| try PrintOps.printRaw(self, s),
                    else => {},
                }
            },
            .PrintVal => {
                const top = try self.popValue();
                try PrintOps.formatHIRValueRaw(self, top);
            },
            .PrintNewline => {
                try PrintOps.printRaw(self, "\n");
            },
            .PrintEnd => {
                // no-op for now (future: flush)
            },
            .PrintInterpolated => |payload| {
                try PrintOps.execPrintInterpolated(self, .{
                    .format_parts = payload.format_parts,
                    .placeholder_indices = payload.placeholder_indices,
                    .argument_count = payload.argument_count,
                    .format_part_ids = payload.format_part_ids,
                });
            },
            .Peek => |payload| {
                try PrintOps.execPeek(self, .{
                    .name = payload.name,
                    .value_type = payload.value_type,
                    .location = payload.location,
                    .union_members = payload.union_members,
                });
            },
            .PeekStruct => |payload| {
                try PrintOps.execPeekStruct(self, .{
                    .type_name = payload.type_name,
                    .field_count = payload.field_count,
                    .field_names = payload.field_names,
                    .field_types = payload.field_types,
                    .location = payload.location,
                    .should_pop_after_peek = payload.should_pop_after_peek,
                });
            },
            .PrintStruct => |payload| {
                try PrintOps.execPrintStruct(self, .{
                    .type_name = payload.type_name,
                    .field_count = payload.field_count,
                    .field_names = payload.field_names,
                    .field_types = payload.field_types,
                    .location = payload.location,
                    .should_pop_after_peek = payload.should_pop_after_peek,
                });
            },
            .Label => |payload| {
                if (payload.id < self.label_cache.len) {
                    self.label_cache[payload.id] = self.ip;
                }
            },
            .Halt => {
                self.running = false;
                self.skip_increment = true;
            },
            .LoadModule => |payload| {
                try self.execLoadModule(payload);
            },
            .Nop => {},
            .AssertFail => |payload| {
                const location_str = try std.fmt.allocPrint(self.allocator, "{s}:{}:{}", .{ payload.location.file, payload.location.range.start_line, payload.location.range.start_col });
                defer self.allocator.free(location_str);

                if (payload.has_message) {
                    const message = try self.stack.pop();
                    const message_str = switch (message.value) {
                        .string => |s| s,
                        else => "Invalid message type",
                    };

                    var stderr_buffer: [1024]u8 = undefined;
                    var stderr_writer = std.fs.File.stderr().writer(&stderr_buffer);
                    const stderr = &stderr_writer.interface;
                    try stderr.print("Assertion failed at {s}:\n{s}\n", .{ location_str, message_str });
                } else {
                    var stderr_buffer: [1024]u8 = undefined;
                    var stderr_writer = std.fs.File.stderr().writer(&stderr_buffer);
                    const stderr = &stderr_writer.interface;
                    try stderr.print("Assertion failed at {s}\n", .{location_str});
                }

                self.running = false;
            },
            .TryBegin => |payload| {
                try self.execTryBegin(payload);
            },
            .TryCatch => |payload| {
                try self.execTryCatch(payload);
            },
            .Throw => |payload| {
                try self.execThrow(payload);
            },
            .EnterScope => |payload| {
                try self.execEnterScope(payload);
            },
            .ExitScope => |payload| {
                try self.execExitScope(payload);
            },
        }
    }

    fn indexLabels(self: *VM) void {
        for (self.bytecode.instructions, 0..) |inst, idx| {
            if (inst == .Label) {
                const id = inst.Label.id;
                if (id < self.label_cache.len) {
                    self.label_cache[id] = idx;
                }
            }
        }
    }

    fn clearScopeStack(self: *VM) void {
        while (self.scope_stack.pop()) |record_val| {
            var record = record_val;
            record.deinit();
        }
    }

    fn currentFrame(self: *VM) VmError!*runtime.Frame {
        if (self.frames.items.len == 0) return error.NoActiveFrame;
        return &self.frames.items[self.frames.items.len - 1];
    }

    fn currentScope(self: *VM) ?*ScopeRecord {
        if (self.scope_stack.items.len == 0) return null;
        return &self.scope_stack.items[self.scope_stack.items.len - 1];
    }

    fn scopeAllocator(self: *VM) std.mem.Allocator {
        if (self.currentScope()) |scope| {
            return scope.arena.allocator();
        }
        return self.allocator;
    }

    fn resolveModuleState(self: *VM, module_id: module.ModuleId) VmError!*runtime.ModuleState {
        const index: usize = @intCast(module_id);
        if (index >= self.module_state.len) return error.MissingModule;
        return &self.module_state[index];
    }

    fn resolveSlot(self: *VM, operand: module.SlotOperand) VmError!runtime.SlotPointer {
        return switch (operand.kind) {
            .local => blk: {
                const frame = try self.currentFrame();
                break :blk frame.pointer(operand.slot);
            },
            .alias => blk: {
                const frame = try self.currentFrame();
                const alias_ptr = frame.pointer(operand.slot);
                const alias_value = alias_ptr.load();

                if (alias_value == .storage_id_ref) {
                    const ref_id = alias_value.storage_id_ref;
                    const original_ptr = self.slotRefFromId(ref_id) catch return error.InvalidAliasReference;
                    break :blk original_ptr;
                }

                if (alias_value == .struct_instance) {
                    break :blk alias_ptr;
                }

                break :blk alias_ptr;
            },
            .module_global, .imported_module => blk: {
                const module_id = operand.module_id orelse return error.MissingModule;
                const state = try self.resolveModuleState(module_id);
                break :blk state.pointer(operand.slot);
            },
            .builtin => return error.UnimplementedInstruction,
        };
    }

    fn trackAlias(self: *VM, slot: module.SlotIndex) VmError!void {
        const scope = self.currentScope() orelse return;
        scope.recordAlias(slot) catch |err| switch (err) {
            error.OutOfMemory => return error.OutOfMemory,
        };
    }

    fn execEnterScope(self: *VM, payload: anytype) VmError!void {
        if (self.skip_next_enter_scope) {
            self.skip_next_enter_scope = false;
            return;
        }

        const record = ScopeRecord.init(self.allocator, payload.scope_id, payload.var_count);
        self.scope_stack.append(record) catch |err| switch (err) {
            error.OutOfMemory => return error.OutOfMemory,
        };
    }

    fn runScopeCleanup(self: *VM, record: *ScopeRecord) VmError!void {
        var d = record.defer_actions.items.len;
        while (d > 0) {
            d -= 1;
            const action = record.defer_actions.items[d];
            action();
        }

        if (self.frames.items.len > 0) {
            var frame = try self.currentFrame();
            for (record.alias_slots.items) |slot| {
                frame.clearAlias(slot);
            }
        }
    }

    fn execExitScope(self: *VM, payload: anytype) VmError!void {
        if (self.scope_stack.items.len == 0) return;

        var idx: ?usize = null;
        var i = self.scope_stack.items.len;
        while (i > 0) {
            i -= 1;
            if (self.scope_stack.items[i].id == payload.scope_id) {
                idx = i;
                break;
            }
        }

        if (idx) |start| {
            while (self.scope_stack.items.len > start) {
                const record_opt = self.scope_stack.pop();
                if (record_opt) |record_val| {
                    var record = record_val;
                    defer record.deinit();
                    try self.runScopeCleanup(&record);
                    if (record.id == payload.scope_id) break;
                } else break;
            }
        }
    }

    fn unwindScopesForCatch(self: *VM, target: usize) VmError!void {
        while (self.scope_stack.pop()) |record_val| {
            var record = record_val;
            defer record.deinit();
            try self.runScopeCleanup(&record);
            if (record.catch_target) |stored| {
                if (stored == target) break;
            }
        }
    }

    fn execTryBegin(self: *VM, payload: anytype) VmError!void {
        if (payload.label_id >= self.label_cache.len) return error.UnimplementedInstruction;
        const target = self.label_cache[payload.label_id];
        self.try_stack.append(target) catch |err| switch (err) {
            error.OutOfMemory => return error.OutOfMemory,
        };
        if (self.currentScope()) |scope| {
            scope.catch_target = target;
        }
    }

    fn execTryCatch(self: *VM, payload: anytype) VmError!void {
        _ = payload;
        _ = self.try_stack.pop();
        if (self.currentScope()) |scope| {
            scope.catch_target = null;
        }
    }

    fn execThrow(self: *VM, payload: anytype) VmError!void {
        _ = payload;
        try self.stack.push(HIRFrame.initNothing());

        if (self.try_stack.items.len == 0) {
            self.reporter.reportRuntimeError(null, ErrorCode.VARIABLE_NOT_FOUND, "No catch block found for throw", .{});
            return ErrorList.TypeError;
        }

        const target = self.try_stack.pop() orelse unreachable;
        try self.unwindScopesForCatch(target);
        self.jumpTo(target);
    }

    fn execQuantifier(self: *VM, is_exists: bool) VmError!void {
        const predicate_value = try self.popValue();
        const array_frame = try self.stack.pop();
        try self.evaluateQuantifier(array_frame, predicate_value, is_exists);
    }

    fn evaluateQuantifier(self: *VM, array_frame: HIRFrame, predicate: HIRValue, is_exists: bool) VmError!void {
        switch (array_frame.value) {
            .array => |arr| {
                var result = if (is_exists) false else true;

                for (arr.elements) |element| {
                    if (std.meta.eql(element, HIRValue.nothing)) break;
                    const predicate_value = try self.evaluatePredicate(predicate, element);
                    const truthy = try self.isTruthy(predicate_value);
                    if (is_exists) {
                        if (truthy) {
                            result = true;
                            break;
                        }
                    } else {
                        if (!truthy) {
                            result = false;
                            break;
                        }
                    }
                }

                try self.stack.push(HIRFrame.initTetra(if (result) 1 else 0));
            },
            .nothing => {
                try self.stack.push(HIRFrame.initTetra(if (is_exists) 0 else 1));
            },
            else => {
                self.reporter.reportRuntimeError(null, ErrorCode.VARIABLE_NOT_FOUND, "Quantifier requires array input", .{});
                return ErrorList.TypeError;
            },
        }
    }

    fn evaluatePredicate(self: *VM, predicate: HIRValue, argument: HIRValue) VmError!HIRValue {
        return switch (predicate) {
            .int => |function_index| {
                if (function_index < 0) {
                    self.reporter.reportRuntimeError(null, ErrorCode.VARIABLE_NOT_FOUND, "Invalid predicate reference", .{});
                    return ErrorList.TypeError;
                }
                return self.evalPredicateFunction(@intCast(function_index), argument);
            },
            .storage_id_ref => |id| {
                const ptr = try self.slotRefFromId(id);
                return self.evaluatePredicate(ptr.load(), argument);
            },
            else => {
                self.reporter.reportRuntimeError(null, ErrorCode.VARIABLE_NOT_FOUND, "Unsupported predicate type for quantifier", .{});
                return ErrorList.TypeError;
            },
        };
    }

    fn evalPredicateFunction(self: *VM, function_index: usize, argument: HIRValue) VmError!HIRValue {
        if (function_index >= self.bytecode.functions.len) return error.UnimplementedInstruction;

        const saved_ip = self.ip;
        const saved_running = self.running;
        const saved_skip = self.skip_increment;
        const saved_stack_len = self.stack.len();
        const saved_frame_depth = self.frames.items.len;

        try self.stack.push(HIRFrame.initFromHIRValue(argument));
        _ = try self.pushFrame(function_index, saved_stack_len, saved_ip);

        const func_ptr = &self.bytecode.functions[function_index];
        if (func_ptr.start_ip >= self.bytecode.instructions.len) return error.UnimplementedInstruction;
        self.ip = func_ptr.start_ip;
        self.running = true;

        while (self.frames.items.len > saved_frame_depth) {
            const inst = self.bytecode.instructions[self.ip];
            self.skip_increment = false;
            try self.execute(inst);
            if (!self.skip_increment) {
                self.ip += 1;
            }
        }

        var result_value: HIRValue = HIRValue.nothing;
        if (self.stack.len() > saved_stack_len) {
            result_value = try self.popValue();
        }

        self.ip = saved_ip;
        self.running = saved_running;
        self.skip_increment = saved_skip;
        self.stack.truncate(saved_stack_len);

        return result_value;
    }

    fn execTailCall(self: *VM, payload: anytype) VmError!void {
        switch (payload.target.call_kind) {
            .LocalFunction, .ModuleFunction => {
                const function_index: usize = @intCast(payload.target.function_index);
                if (function_index >= self.bytecode.functions.len) return error.UnimplementedInstruction;

                const arg_count: usize = @intCast(payload.arg_count);
                if (self.stack.len() < arg_count) return error.UnimplementedInstruction;
                if (self.frames.items.len == 0) return error.NoActiveFrame;

                const frame_index = self.frames.items.len - 1;
                const dest_start = self.frames.items[frame_index].getStackBase();
                _ = self.frames.items[frame_index].getSlotRefBase();
                const return_ip = self.frames.items[frame_index].getReturnIp();

                const args_start = self.stack.len() - arg_count;
                if (args_start != dest_start) {
                    var offset: usize = 0;
                    while (offset < arg_count) : (offset += 1) {
                        self.stack.values[dest_start + offset] = self.stack.values[args_start + offset];
                    }
                }
                self.stack.sp = dest_start + arg_count;

                var old_frame = self.frames.pop() orelse return error.NoActiveFrame;
                old_frame.deinit();

                _ = try self.pushFrame(function_index, dest_start, return_ip);

                self.skip_next_enter_scope = true;
                const func_ptr = &self.bytecode.functions[function_index];
                if (func_ptr.start_ip >= self.bytecode.instructions.len) return error.UnimplementedInstruction;
                self.ip = func_ptr.start_ip;
                self.skip_increment = true;
            },
            .BuiltinFunction => return error.UnimplementedInstruction,
        }
    }

    fn execBuiltin(self: *VM, name: []const u8, arg_count_raw: u32) VmError!void {
        const arg_count: usize = @intCast(arg_count_raw);

        if (std.mem.eql(u8, name, "length")) {
            if (arg_count != 1) return error.UnimplementedInstruction;
            const value = try self.stack.pop();
            switch (value.value) {
                .array => |arr| {
                    var length: usize = 0;
                    for (arr.elements) |elem| {
                        if (std.meta.eql(elem, HIRValue.nothing)) break;
                        length += 1;
                    }
                    try self.stack.push(HIRFrame.initInt(@intCast(length)));
                },
                else => return self.reporter.reportRuntimeError(null, ErrorCode.VARIABLE_NOT_FOUND, "length: argument must be array", .{}),
            }
            return;
        }

        if (std.mem.eql(u8, name, "push")) {
            if (arg_count != 2) return error.UnimplementedInstruction;
            const element = try self.stack.pop();
            const array_frame = try self.stack.pop();

            switch (array_frame.value) {
                .array => |arr| {
                    var mutable_arr = arr;
                    var length: usize = 0;
                    for (mutable_arr.elements) |elem| {
                        if (std.meta.eql(elem, HIRValue.nothing)) break;
                        length += 1;
                    }

                    if (length >= mutable_arr.capacity) {
                        const new_capacity = @max(mutable_arr.capacity * 2, mutable_arr.capacity + 1);
                        var new_elements = try self.allocator.alloc(HIRValue, new_capacity);
                        @memcpy(new_elements[0..mutable_arr.elements.len], mutable_arr.elements);
                        self.allocator.free(mutable_arr.elements);
                        mutable_arr.elements = new_elements;
                        mutable_arr.capacity = @intCast(new_capacity);
                    }

                    mutable_arr.elements[length] = element.value;
                    if (length + 1 < mutable_arr.elements.len) {
                        mutable_arr.elements[length + 1] = HIRValue.nothing;
                    }

                    try self.stack.push(HIRFrame.initFromHIRValue(HIRValue{ .array = mutable_arr }));
                },
                else => return self.reporter.reportRuntimeError(null, ErrorCode.VARIABLE_NOT_FOUND, "push: target must be array", .{}),
            }
            return;
        }

        if (std.mem.eql(u8, name, "safeAdd")) {
            if (arg_count != 2) return error.UnimplementedInstruction;
            const b = try self.stack.pop();
            const a = try self.stack.pop();

            const a_int = switch (a.value) {
                .int => |i| i,
                .byte => |u| @as(i64, u),
                else => return self.reporter.reportRuntimeError(null, ErrorCode.VARIABLE_NOT_FOUND, "safeAdd: first argument must be integer", .{}),
            };

            const b_int = switch (b.value) {
                .int => |i| i,
                .byte => |u| @as(i64, u),
                else => return self.reporter.reportRuntimeError(null, ErrorCode.VARIABLE_NOT_FOUND, "safeAdd: second argument must be integer", .{}),
            };

            const limit = 255;
            if (a_int > limit or b_int > limit or a_int < 0 or b_int < 0) {
                try self.stack.push(HIRFrame.initInt(-1));
                return;
            }

            const result = std.math.add(i64, a_int, b_int) catch {
                try self.stack.push(HIRFrame.initInt(-1));
                return;
            };

            try self.stack.push(HIRFrame.initInt(result));
            return;
        }

        if (std.mem.eql(u8, name, "power") or std.mem.eql(u8, name, "powi")) {
            if (arg_count != 2) return error.UnimplementedInstruction;
            const exponent = try self.stack.pop();
            const base = try self.stack.pop();

            if (std.mem.eql(u8, name, "powi")) {
                const base_int = switch (base.value) {
                    .int => |i| i,
                    .byte => |b| @as(i64, b),
                    else => return self.reporter.reportRuntimeError(null, ErrorCode.VARIABLE_NOT_FOUND, "powi: base must be integer", .{}),
                };
                const exp_int = switch (exponent.value) {
                    .int => |i| i,
                    .byte => |b| @as(i64, b),
                    else => return self.reporter.reportRuntimeError(null, ErrorCode.VARIABLE_NOT_FOUND, "powi: exponent must be integer", .{}),
                };
                const result = std.math.pow(i64, base_int, @intCast(exp_int));
                try self.stack.push(HIRFrame.initInt(result));
                return;
            }

            const base_float = switch (base.value) {
                .int => |i| @as(f64, @floatFromInt(i)),
                .float => |f| f,
                .byte => |b| @as(f64, @floatFromInt(b)),
                else => return self.reporter.reportRuntimeError(null, ErrorCode.VARIABLE_NOT_FOUND, "power: base must be numeric", .{}),
            };

            const exponent_float = switch (exponent.value) {
                .int => |i| @as(f64, @floatFromInt(i)),
                .float => |f| f,
                .byte => |b| @as(f64, @floatFromInt(b)),
                else => return self.reporter.reportRuntimeError(null, ErrorCode.VARIABLE_NOT_FOUND, "power: exponent must be numeric", .{}),
            };

            const result = std.math.pow(f64, base_float, exponent_float);
            try self.stack.push(HIRFrame.initFloat(result));
            return;
        }

        if (std.mem.eql(u8, name, "exists_quantifier_gt") or std.mem.eql(u8, name, "exists_quantifier_eq")) {
            if (arg_count != 2) return error.UnimplementedInstruction;
            const comparison_value = try self.stack.pop();
            const array_frame = try self.stack.pop();
            const is_equality = std.mem.eql(u8, name, "exists_quantifier_eq");

            switch (array_frame.value) {
                .array => |arr| {
                    var found = false;
                    for (arr.elements) |elem| {
                        if (std.meta.eql(elem, HIRValue.nothing)) break;
                        const satisfies = switch (elem) {
                            .int => |elem_int| switch (comparison_value.value) {
                                .int => |comp_int| if (is_equality) elem_int == comp_int else elem_int > comp_int,
                                else => false,
                            },
                            .float => |elem_float| switch (comparison_value.value) {
                                .float => |comp_float| if (is_equality) elem_float == comp_float else elem_float > comp_float,
                                .int => |comp_int| blk: {
                                    const comp_float = @as(f64, @floatFromInt(comp_int));
                                    break :blk if (is_equality) elem_float == comp_float else elem_float > comp_float;
                                },
                                else => false,
                            },
                            .string => |elem_str| switch (comparison_value.value) {
                                .string => |comp_str| if (is_equality) std.mem.eql(u8, elem_str, comp_str) else false,
                                else => false,
                            },
                            else => false,
                        };
                        if (satisfies) {
                            found = true;
                            break;
                        }
                    }
                    try self.stack.push(HIRFrame.initTetra(if (found) 1 else 0));
                },
                else => return self.reporter.reportRuntimeError(null, ErrorCode.VARIABLE_NOT_FOUND, "exists_quantifier: argument must be array", .{}),
            }
            return;
        }

        if (std.mem.eql(u8, name, "forall_quantifier_gt") or std.mem.eql(u8, name, "forall_quantifier_eq")) {
            if (arg_count != 2) return error.UnimplementedInstruction;
            const comparison_value = try self.stack.pop();
            const array_frame = try self.stack.pop();
            const is_equality = std.mem.eql(u8, name, "forall_quantifier_eq");

            switch (array_frame.value) {
                .array => |arr| {
                    var all_satisfy = true;
                    var has_elements = false;
                    for (arr.elements) |elem| {
                        if (std.meta.eql(elem, HIRValue.nothing)) break;
                        has_elements = true;
                        const satisfies = switch (elem) {
                            .int => |elem_int| switch (comparison_value.value) {
                                .int => |comp_int| if (is_equality) elem_int == comp_int else elem_int > comp_int,
                                else => false,
                            },
                            .float => |elem_float| switch (comparison_value.value) {
                                .float => |comp_float| if (is_equality) elem_float == comp_float else elem_float > comp_float,
                                .int => |comp_int| blk: {
                                    const comp_float = @as(f64, @floatFromInt(comp_int));
                                    break :blk if (is_equality) elem_float == comp_float else elem_float > comp_float;
                                },
                                else => false,
                            },
                            .string => |elem_str| switch (comparison_value.value) {
                                .string => |comp_str| if (is_equality) std.mem.eql(u8, elem_str, comp_str) else false,
                                else => false,
                            },
                            else => false,
                        };
                        if (!satisfies) {
                            all_satisfy = false;
                            break;
                        }
                    }
                    try self.stack.push(HIRFrame.initTetra(if (!has_elements or all_satisfy) 1 else 0));
                },
                else => return self.reporter.reportRuntimeError(null, ErrorCode.VARIABLE_NOT_FOUND, "forall_quantifier: argument must be array", .{}),
            }
            return;
        }

        if (std.mem.eql(u8, name, "input")) {
            if (arg_count != 0) return error.UnimplementedInstruction;
            var stdin_buffer: [4096]u8 = undefined;
            var stdin_reader = std.fs.File.stdin().reader(&stdin_buffer);
            const stdin = &stdin_reader.interface;

            const line = stdin.takeDelimiterExclusive('\n') catch {
                return self.reporter.reportRuntimeError(null, ErrorCode.VARIABLE_NOT_FOUND, "input: failed to read", .{});
            };

            var trimmed = line;
            while (trimmed.len > 0 and (trimmed[trimmed.len - 1] == '\n' or trimmed[trimmed.len - 1] == '\r')) {
                trimmed = trimmed[0 .. trimmed.len - 1];
            }

            const duped = try self.allocator.dupe(u8, trimmed);
            try self.stack.push(HIRFrame.initString(duped));
            return;
        }

        if (std.mem.eql(u8, name, "os")) {
            if (arg_count != 0) return error.UnimplementedInstruction;
            const os_name = @tagName(@import("builtin").os.tag);
            const duped = try self.allocator.dupe(u8, os_name);
            try self.stack.push(HIRFrame.initString(duped));
            return;
        }

        if (std.mem.eql(u8, name, "arch")) {
            if (arg_count != 0) return error.UnimplementedInstruction;
            const arch_name = @tagName(@import("builtin").cpu.arch);
            const duped = try self.allocator.dupe(u8, arch_name);
            try self.stack.push(HIRFrame.initString(duped));
            return;
        }

        if (std.mem.eql(u8, name, "time")) {
            if (arg_count != 0) return error.UnimplementedInstruction;
            const timestamp = std.time.timestamp();
            try self.stack.push(HIRFrame.initInt(timestamp));
            return;
        }

        if (std.mem.eql(u8, name, "tick")) {
            if (arg_count != 0) return error.UnimplementedInstruction;
            const ns = std.time.nanoTimestamp();
            try self.stack.push(HIRFrame.initInt(@intCast(ns)));
            return;
        }

        if (std.mem.eql(u8, name, "build")) {
            // New signature: @build(src: string, out: string, arch: string, os: string, debug: tetra)
            if (arg_count != 5) return error.UnimplementedInstruction;
            const debug_frame = try self.stack.pop();
            const os_frame = try self.stack.pop();
            const arch_frame = try self.stack.pop();
            const out_path_frame = try self.stack.pop();
            const src_path_frame = try self.stack.pop();

            const src_path = switch (src_path_frame.value) {
                .string => |s| s,
                else => return self.reporter.reportRuntimeError(null, ErrorCode.TYPE_MISMATCH, "build: first argument must be string", .{}),
            };
            const out_path = switch (out_path_frame.value) {
                .string => |s| s,
                else => return self.reporter.reportRuntimeError(null, ErrorCode.TYPE_MISMATCH, "build: second argument must be string", .{}),
            };
            const arch_str = switch (arch_frame.value) {
                .string => |s| s,
                else => return self.reporter.reportRuntimeError(null, ErrorCode.TYPE_MISMATCH, "build: third argument (arch) must be string", .{}),
            };
            const os_str = switch (os_frame.value) {
                .string => |s| s,
                else => return self.reporter.reportRuntimeError(null, ErrorCode.TYPE_MISMATCH, "build: fourth argument (os) must be string", .{}),
            };
            const debug_val: u8 = switch (debug_frame.value) {
                .tetra => |t| t,
                else => 0,
            };

            var arena = std.heap.ArenaAllocator.init(self.allocator);
            defer arena.deinit();
            const a = arena.allocator();

            // Resolve doxa executable path: self-exe -> DOXA_EXE env -> ./zig-out/bin/doxa -> PATH "doxa"
            var doxa_path: []const u8 = "doxa";
            const self_path_opt = std.fs.selfExePathAlloc(a) catch null;
            if (self_path_opt) |self_path| {
                doxa_path = self_path;
            } else {
                const env_path_opt: ?[]const u8 = std.process.getEnvVarOwned(a, "DOXA_EXE") catch null;
                if (env_path_opt) |env_path| {
                    doxa_path = env_path;
                } else {
                    const candidate = try std.fs.path.join(a, &[_][]const u8{ ".", "zig-out", "bin", "doxa" });
                    const f = std.fs.cwd().openFile(candidate, .{}) catch null;
                    if (f) |fh| {
                        fh.close();
                        doxa_path = candidate;
                    }
                }
            }

            // Map tetra to opt level flags: false(0)->-O-1, true(1)->-O0, both(2)->-O1, neither(3)->-O-1
            const opt_flag = switch (debug_val) {
                0 => "-O-1",
                1 => "-O0",
                2 => "-O1",
                else => "-O-1",
            };

            // Build CLI args
            var args_builder = std.array_list.Managed([]const u8).init(a);
            defer args_builder.deinit();
            try args_builder.append(doxa_path);
            try args_builder.append("compile");
            try args_builder.append(src_path);
            try args_builder.append("-o");
            try args_builder.append(out_path);
            try args_builder.append(opt_flag);
            if (arch_str.len > 0) try args_builder.append(try std.fmt.allocPrint(a, "--arch={s}", .{arch_str}));
            if (os_str.len > 0) try args_builder.append(try std.fmt.allocPrint(a, "--os={s}", .{os_str}));

            var child = std.process.Child.init(args_builder.items, a);
            child.cwd = ".";
            child.stdout_behavior = .Inherit;
            child.stderr_behavior = .Inherit;
            const term = child.spawnAndWait() catch {
                try self.stack.push(HIRFrame.initInt(1));
                return;
            };
            switch (term) {
                .Exited => |code| {
                    try self.stack.push(HIRFrame.initInt(@intCast(code)));
                    return;
                },
                else => {
                    try self.stack.push(HIRFrame.initInt(1));
                    return;
                },
            }
        }

        if (std.mem.eql(u8, name, "exit")) {
            if (arg_count > 1) return error.UnimplementedInstruction;
            var exit_code: i64 = 0;
            if (arg_count == 1) {
                const value = try self.stack.pop();
                exit_code = switch (value.value) {
                    .int => |i| i,
                    .byte => |b| @as(i64, b),
                    else => return self.reporter.reportRuntimeError(null, ErrorCode.VARIABLE_NOT_FOUND, "@exit: argument must be an integer", .{}),
                };
            }
            self.running = false;
            self.skip_increment = true;
            try self.stack.push(HIRFrame.initInt(exit_code));
            return;
        }

        if (std.mem.eql(u8, name, "sleep")) {
            if (arg_count != 1) return error.UnimplementedInstruction;
            const duration_frame = try self.stack.pop();
            const duration_ns = switch (duration_frame.value) {
                .int => |i| if (i < 0) {
                    self.reporter.reportRuntimeError(null, ErrorCode.VARIABLE_NOT_FOUND, "@sleep: argument must be non-negative", .{});
                    return ErrorList.TypeError;
                } else @as(u64, @intCast(i * std.time.ns_per_ms)),
                else => {
                    self.reporter.reportRuntimeError(null, ErrorCode.VARIABLE_NOT_FOUND, "@sleep: argument must be an integer", .{});
                    return ErrorList.TypeError;
                },
            };
            std.Thread.sleep(duration_ns);
            try self.stack.push(HIRFrame.initNothing());
            return;
        }

        if (std.mem.eql(u8, name, "random")) {
            if (arg_count != 0) return error.UnimplementedInstruction;
            const rand_value = std.crypto.random.float(f64);
            try self.stack.push(HIRFrame.initFloat(rand_value));
            return;
        }

        if (std.mem.eql(u8, name, "byte")) {
            if (arg_count != 1) return error.UnimplementedInstruction;
            const value = try self.stack.pop();

            const byte_val: u8 = switch (value.value) {
                .string => |s| if (s.len > 0) s[0] else 0,
                .int => |i| if (i >= 0 and i <= 255) @as(u8, @intCast(i)) else 0,
                .byte => |byte_val| byte_val,
                .float => |f| if (f >= 0 and f <= 255) @intFromFloat(f) else 0,
                else => 0,
            };

            try self.stack.push(HIRFrame.initByte(byte_val));
            return;
        }

        self.reporter.reportRuntimeError(null, ErrorCode.VARIABLE_NOT_FOUND, "Unknown built-in function: {s}", .{name});
        return ErrorList.TypeError;
    }

    fn execStructNew(self: *VM, payload: anytype) VmError!void {
        const alloc = self.scopeAllocator();
        var fields = try alloc.alloc(HIRStructField, payload.field_count);
        errdefer alloc.free(fields);

        var idx: usize = payload.field_count;
        while (idx > 0) {
            idx -= 1;
            const field_name_frame = try self.stack.pop();
            const field_value_frame = try self.stack.pop();

            const name_str = switch (field_name_frame.value) {
                .string => |s| s,
                else => return self.reporter.reportRuntimeError(null, ErrorCode.VARIABLE_NOT_FOUND, "Expected string for field name, got {s}", .{@tagName(field_name_frame.value)}),
            };

            const interned_name = try self.string_interner.intern(name_str);
            fields[idx] = HIRStructField{
                .name = interned_name,
                .value = field_value_frame.value,
                .field_type = payload.field_types[idx],
                .path = null,
            };
        }

        const type_name_copy = try alloc.dupe(u8, payload.type_name);
        errdefer alloc.free(type_name_copy);

        const struct_value = HIRValue{ .struct_instance = HIRStruct{
            .type_name = type_name_copy,
            .fields = fields,
            .field_name = null,
            .path = null,
        } };

        try self.stack.push(HIRFrame.initFromHIRValue(struct_value));
    }

    fn execEnumNew(self: *VM, payload: anytype) VmError!void {
        const type_name_copy = try self.allocator.dupe(u8, payload.enum_name);
        errdefer self.allocator.free(type_name_copy);
        const variant_name_copy = try self.allocator.dupe(u8, payload.variant_name);
        errdefer self.allocator.free(variant_name_copy);

        const enum_value = HIRValue{ .enum_variant = .{
            .type_name = type_name_copy,
            .variant_name = variant_name_copy,
            .variant_index = payload.variant_index,
            .path = null,
        } };

        try self.stack.push(HIRFrame.initFromHIRValue(enum_value));
    }

    fn execStoreFieldName(self: *VM, payload: anytype) VmError!void {
        const frame = try self.stack.pop();

        switch (frame.value) {
            .struct_instance => |struct_inst| {
                var has_empty = false;
                for (struct_inst.fields) |field| {
                    if (field.name.len == 0) {
                        has_empty = true;
                        break;
                    }
                }

                if (!has_empty) {
                    try self.stack.push(frame);
                    return;
                }

                const interned_name = try self.string_interner.intern(payload.field_name);
                for (struct_inst.fields) |*field| {
                    if (field.name.len == 0) {
                        field.name = interned_name;
                        break;
                    }
                }

                const modified = HIRFrame.initFromHIRValue(HIRValue{ .struct_instance = struct_inst });
                try self.stack.push(modified);
            },
            else => {
                self.reporter.reportRuntimeError(null, ErrorCode.VARIABLE_NOT_FOUND, "Cannot store field name on non-struct value: {s}", .{@tagName(frame.value)});
                return ErrorList.TypeError;
            },
        }
    }

    fn execGetField(self: *VM, payload: anytype) VmError!void {
        const frame = try self.stack.pop();

        // Handle enum type access (e.g., Token.INT_LITERAL)
        if (frame.value == .string) {
            const type_name = frame.value.string;
            const field_name = payload.field_name;

            // Check if this is a known enum type by looking at the custom type registry
            if (self.custom_type_registry.get(type_name)) |custom_type| {
                if (custom_type.kind == .Enum) {
                    if (custom_type.enum_variants) |variants| {
                        // Find the variant index
                        for (variants) |variant| {
                            if (std.mem.eql(u8, variant.name, field_name)) {
                                // Create enum variant value
                                const enum_value = HIRValue{
                                    .enum_variant = .{
                                        .type_name = try self.allocator.dupe(u8, type_name),
                                        .variant_name = try self.allocator.dupe(u8, field_name),
                                        .variant_index = variant.index,
                                        .path = null,
                                    },
                                };
                                try self.stack.push(HIRFrame{ .value = enum_value });
                                return;
                            }
                        }
                    }
                }
            }

            // If not found as enum, fall through to error
            return self.reporter.reportRuntimeError(null, ErrorCode.VARIABLE_NOT_FOUND, "Cannot get field '{s}' from string value '{s}'", .{ field_name, type_name });
        }

        if (frame.value != .struct_instance) {
            return self.reporter.reportRuntimeError(null, ErrorCode.VARIABLE_NOT_FOUND, "Cannot get field from non-struct value: {s}", .{@tagName(frame.value)});
        }

        const interned_name = try self.string_interner.intern(payload.field_name);

        for (frame.value.struct_instance.fields) |field| {
            if ((field.name.ptr == interned_name.ptr and field.name.len == interned_name.len) or std.mem.eql(u8, field.name, interned_name)) {
                var field_value = field.value;
                if (field_value == .struct_instance) {
                    const path_or_type: []const u8 = if (frame.value.struct_instance.path) |p| p else frame.value.struct_instance.type_name;
                    const scope_alloc = self.scopeAllocator();
                    const tmp = try std.fmt.allocPrint(scope_alloc, "{s}.{s}", .{ path_or_type, field.name });
                    const interned = try self.string_interner.intern(tmp);
                    scope_alloc.free(tmp);
                    field_value.struct_instance.path = interned;
                }

                try self.stack.push(HIRFrame{ .value = field_value });
                return;
            }
        }

        return self.reporter.reportRuntimeError(null, ErrorCode.VARIABLE_NOT_FOUND, "Cannot get field from non-struct value: {s}", .{@tagName(frame.value)});
    }

    fn execSetField(self: *VM, payload: anytype) VmError!void {
        const value_frame = try self.stack.pop();
        const struct_frame = try self.stack.pop();

        if (struct_frame.value != .struct_instance) {
            return self.reporter.reportRuntimeError(null, ErrorCode.VARIABLE_NOT_FOUND, "Cannot set field on non-struct value: {s}", .{@tagName(struct_frame.value)});
        }

        switch (struct_frame.value) {
            .struct_instance => |struct_inst| {
                for (struct_inst.fields) |*field| {
                    if (std.mem.eql(u8, field.name, payload.field_name)) {
                        field.value = value_frame.value;
                        const modified = HIRFrame.initFromHIRValue(HIRValue{ .struct_instance = struct_inst });
                        try self.stack.push(modified);
                        return;
                    }
                }

                return self.reporter.reportRuntimeError(null, ErrorCode.VARIABLE_NOT_FOUND, "Field '{s}' not found in struct '{s}'", .{ payload.field_name, struct_inst.type_name });
            },
            else => return self.reporter.reportRuntimeError(null, ErrorCode.VARIABLE_NOT_FOUND, "Cannot set field on non-struct value: {s}", .{@tagName(struct_frame.value)}),
        }
    }

    fn execMap(self: *VM, payload: anytype) VmError!void {
        const alloc = self.scopeAllocator();
        var entries = try alloc.alloc(HIRMapEntry, payload.entries.len);
        errdefer alloc.free(entries);

        var reverse_i = entries.len;
        while (reverse_i > 0) {
            reverse_i -= 1;
            const value_frame = try self.stack.pop();
            const key_frame = try self.stack.pop();

            const normalized_key = switch (key_frame.value) {
                .string => |s| HIRValue{ .string = try self.string_interner.intern(s) },
                else => key_frame.value,
            };

            entries[reverse_i] = HIRMapEntry{
                .key = normalized_key,
                .value = value_frame.value,
                .path = null,
            };
        }

        const map_value = HIRValue{ .map = HIRMap{
            .entries = entries,
            .key_type = toHIRType(payload.key_type),
            .value_type = toHIRType(payload.value_type),
            .path = null,
        } };

        try self.stack.push(HIRFrame.initFromHIRValue(map_value));
    }

    fn execMapGet(self: *VM) VmError!void {
        var key_frame = try self.stack.pop();
        switch (key_frame.value) {
            .string => |s| {
                const interned = try self.string_interner.intern(s);
                key_frame.value = HIRValue{ .string = interned };
            },
            else => {},
        }

        const map_frame = try self.stack.pop();

        switch (map_frame.value) {
            .map => |map_value| {
                for (map_value.entries) |entry| {
                    const keys_match = switch (entry.key) {
                        .string => |entry_str| switch (key_frame.value) {
                            .string => |key_str| ((entry_str.ptr == key_str.ptr and entry_str.len == key_str.len) or std.mem.eql(u8, entry_str, key_str)),
                            else => false,
                        },
                        .int => |entry_int| switch (key_frame.value) {
                            .int => |key_int| entry_int == key_int,
                            else => false,
                        },
                        else => false,
                    };

                    if (keys_match) {
                        try self.stack.push(HIRFrame.initFromHIRValue(entry.value));
                        return;
                    }
                }

                try self.stack.push(HIRFrame.initFromHIRValue(HIRValue.nothing));
            },
            else => {
                return self.reporter.reportRuntimeError(null, ErrorCode.VARIABLE_NOT_FOUND, "Cannot get from non-map value: {s}", .{@tagName(map_frame.value)});
            },
        }
    }

    fn execMapSet(self: *VM, payload: anytype) VmError!void {
        const alloc = self.scopeAllocator();
        const value_frame = try self.stack.pop();
        var key_frame = try self.stack.pop();
        switch (key_frame.value) {
            .string => |s| {
                const interned = try self.string_interner.intern(s);
                key_frame.value = HIRValue{ .string = interned };
            },
            else => {},
        }
        const map_frame = try self.stack.pop();

        switch (map_frame.value) {
            .map => |orig| {
                var entries = try alloc.alloc(HIRMapEntry, orig.entries.len);
                errdefer alloc.free(entries);
                @memcpy(entries, orig.entries);

                var updated = false;
                for (entries) |*entry| {
                    const keys_match = switch (entry.key) {
                        .string => |entry_str| switch (key_frame.value) {
                            .string => |key_str| ((entry_str.ptr == key_str.ptr and entry_str.len == key_str.len) or std.mem.eql(u8, entry_str, key_str)),
                            else => false,
                        },
                        .int => |entry_int| switch (key_frame.value) {
                            .int => |key_int| entry_int == key_int,
                            else => false,
                        },
                        else => false,
                    };

                    if (keys_match) {
                        entry.value = value_frame.value;
                        updated = true;
                        break;
                    }
                }

                if (!updated) {
                    var new_entries = try alloc.alloc(HIRMapEntry, entries.len + 1);
                    @memcpy(new_entries[0..entries.len], entries);
                    new_entries[entries.len] = HIRMapEntry{ .key = key_frame.value, .value = value_frame.value, .path = null };
                    alloc.free(entries);
                    entries = new_entries;
                }

                const new_map = HIRValue{ .map = HIRMap{
                    .entries = entries,
                    .key_type = toHIRType(payload.key_type),
                    .value_type = orig.value_type,
                    .path = null,
                } };

                try self.stack.push(HIRFrame.initFromHIRValue(new_map));
            },
            else => {
                return self.reporter.reportRuntimeError(null, ErrorCode.VARIABLE_NOT_FOUND, "Cannot set index on non-map value: {s}", .{@tagName(map_frame.value)});
            },
        }
    }

    fn cacheSlotPointer(self: *VM, pointer: runtime.SlotPointer) !u32 {
        if (self.slot_refs.items.len >= std.math.maxInt(u32)) {
            return error.OutOfMemory;
        }
        try self.slot_refs.append(pointer);
        return @intCast(self.slot_refs.items.len - 1);
    }

    fn slotRefFromId(self: *VM, id: u32) !runtime.SlotPointer {
        const index: usize = @intCast(id);
        if (index >= self.slot_refs.items.len) return error.InvalidAliasReference;
        return self.slot_refs.items[index];
    }

    fn jumpTo(self: *VM, target: usize) void {
        if (target >= self.bytecode.instructions.len) {
            self.running = false;
            return;
        }
        self.ip = target;
        self.skip_increment = true;
    }

    fn isTruthy(self: *VM, value: HIRValue) !bool {
        return switch (value) {
            .tetra => |v| switch (v) {
                0 => false, // false
                1 => true, // true
                2 => true, // both (is true)
                3 => false, // neither (is not true)
                else => false,
            },
            else => {
                self.reporter.reportRuntimeError(null, ErrorCode.NOT_TRUTHY, "Truthy value cannot be determined for value: {s}, use a tetra instead", .{@tagName(value)});
                return ErrorList.TypeError;
            },
        };
    }

    fn resolveFunctionIndex(self: *VM, function_index: usize, qualified_name: []const u8) usize {
        if (function_index < self.bytecode.functions.len) {
            const func = &self.bytecode.functions[function_index];
            if (std.mem.eql(u8, func.qualified_name, qualified_name)) {
                return function_index;
            }
        }

        for (self.bytecode.functions, 0..) |func, idx| {
            if (std.mem.eql(u8, func.qualified_name, qualified_name)) {
                return idx;
            }
        }

        return function_index;
    }

    fn handleCall(self: *VM, payload: anytype) VmError!void {
        switch (payload.target.call_kind) {
            .LocalFunction, .ModuleFunction => {
                const function_index = self.resolveFunctionIndex(payload.target.function_index, payload.target.qualified_name);
                const func_ptr = &self.bytecode.functions[function_index];
                const return_ip = self.ip;

                const stack_base = self.stack.len();

                _ = try self.pushFrame(function_index, stack_base, return_ip);

                if (func_ptr.start_ip >= self.bytecode.instructions.len) return error.UnimplementedInstruction;
                self.ip = func_ptr.start_ip;
                self.skip_increment = true;
            },
            .BuiltinFunction => try self.execBuiltin(payload.target.qualified_name, payload.arg_count),
        }
    }

    fn handleReturn(self: *VM, payload: anytype) VmError!void {
        var return_value: ?HIRValue = null;
        if (payload.has_value) {
            return_value = try self.popValue();
        } else {}

        if (self.frames.items.len == 0) {
            self.running = false;
            self.skip_increment = true;
            if (return_value) |val| try self.pushValue(val);
            return;
        }

        var frame = self.frames.pop() orelse return error.NoActiveFrame;
        defer frame.deinit();

        const caller_exists = self.frames.items.len > 0;
        var promoted_value: ?HIRValue = null;
        if (return_value) |val| {
            if (caller_exists) {
                const dest_alloc = self.frames.items[self.frames.items.len - 1].arena.allocator();
                promoted_value = try self.deepCopyValueToAllocator(dest_alloc, val);
            } else {
                // No caller: promote to VM allocator; program likely ends soon
                promoted_value = try self.deepCopyValueToAllocator(self.allocator, val);
            }
        }

        self.stack.truncate(frame.getStackBase());

        if (promoted_value) |pv| {
            try self.stack.pushValue(pv);
        } else {
            try self.stack.pushValue(HIRValue.nothing);
        }

        if (self.frames.items.len == 0) {
            self.running = false;
            self.skip_increment = true;
        } else {
            const next_ip = frame.getReturnIp() + 1;
            self.ip = next_ip;
            self.skip_increment = true;
        }
    }

    fn invokeFunction(self: *VM, function_index: usize, arg_count_raw: u32) VmError!void {
        if (function_index >= self.bytecode.functions.len) return error.UnimplementedInstruction;
        const func_ptr = &self.bytecode.functions[function_index];
        const arg_count: usize = @intCast(arg_count_raw);
        if (self.stack.len() < arg_count) return error.UnimplementedInstruction;
        const stack_base = self.stack.len() - arg_count;
        const return_ip = self.ip;
        _ = try self.pushFrame(function_index, stack_base, return_ip);
        if (func_ptr.start_ip >= self.bytecode.instructions.len) return error.UnimplementedInstruction;
        self.ip = func_ptr.start_ip;
        self.skip_increment = true;
    }

    fn pushFrame(self: *VM, function_index: usize, stack_base: usize, return_ip: usize) VmError!*runtime.Frame {
        const func_ptr = &self.bytecode.functions[function_index];
        var frame = try runtime.Frame.init(self.allocator, func_ptr, stack_base, self.slot_refs.items.len, return_ip);
        errdefer frame.deinit();

        const required_locals = 1000;

        frame.arena.deinit();
        frame.arena = std.heap.ArenaAllocator.init(self.allocator);
        const frame_alloc = frame.arena.allocator();
        frame.locals = try frame_alloc.alloc(HIRValue, required_locals);
        frame.alias_refs = try frame_alloc.alloc(?runtime.SlotPointer, required_locals);
        for (frame.locals) |*slot| slot.* = HIRValue.nothing;
        for (frame.alias_refs) |*entry| entry.* = null;

        frame.function.local_var_count = @intCast(required_locals);

        self.frames.append(frame) catch |err| {
            frame.deinit();
            return err;
        };
        return &self.frames.items[self.frames.items.len - 1];
    }

    fn prepareEntryFrame(self: *VM) VmError!void {
        if (self.frames.items.len != 0) return;

        self.ip = 0;

        var entry_function_index: ?usize = null;
        for (self.bytecode.functions, 0..) |func, idx| {
            if (func.is_entry) {
                entry_function_index = idx;
                break;
            }
        }

        if (entry_function_index) |entry_idx| {
            _ = try self.pushFrame(entry_idx, self.stack.len(), 0);
        }

        if (self.bytecode.functions.len == 0) {
            const required_locals = 1000;

            var dummy_function = module.BytecodeFunction{
                .name = "main",
                .qualified_name = "main",
                .module_id = 0,
                .arity = 0,
                .return_type = .Nothing,
                .start_label = "main",
                .body_label = null,
                .start_ip = 0,
                .body_ip = null,
                .local_var_count = required_locals,
                .is_entry = true,
                .param_types = &[_]module.BytecodeType{},
                .param_is_alias = &[_]bool{},
            };

            var frame = try runtime.Frame.init(self.allocator, &dummy_function, self.stack.len(), self.slot_refs.items.len, 0);

            frame.allocator.free(frame.locals);
            frame.allocator.free(frame.alias_refs);
            frame.locals = try frame.allocator.alloc(HIRValue, required_locals);
            frame.alias_refs = try frame.allocator.alloc(?runtime.SlotPointer, required_locals);
            for (frame.locals) |*slot| slot.* = HIRValue.nothing;
            for (frame.alias_refs) |*entry| entry.* = null;

            self.frames.append(frame) catch |err| {
                frame.deinit();
                return err;
            };
            self.ip = 0;
            return;
        }

        var frame = try runtime.Frame.init(self.allocator, &self.bytecode.functions[0], self.stack.len(), self.slot_refs.items.len, 0);
        const required_locals = 1000;
        frame.arena.deinit();
        frame.arena = std.heap.ArenaAllocator.init(self.allocator);
        const frame_alloc2 = frame.arena.allocator();
        frame.locals = try frame_alloc2.alloc(HIRValue, required_locals);
        frame.alias_refs = try frame_alloc2.alloc(?runtime.SlotPointer, required_locals);
        for (frame.locals) |*slot| slot.* = HIRValue.nothing;
        for (frame.alias_refs) |*entry| entry.* = null;

        frame.function.local_var_count = @intCast(required_locals);

        self.frames.append(frame) catch |err| {
            frame.deinit();
            return err;
        };
        self.ip = 0;
    }

    fn toHIRType(type_tag: module.BytecodeType) hir_types.HIRType {
        return switch (type_tag) {
            .Int => .Int,
            .Byte => .Byte,
            .Float => .Float,
            .String => .String,
            .Tetra => .Tetra,
            .Nothing => .Nothing,
            // Complex bytecode tags are not standalone HIR types; callers should
            // construct full HIR types with the required nested info where needed.
            // Array requires payload; callers must use toHIRTypeWithNested
            .Array => unreachable,
            // Use placeholder IDs for nominal types; actual IDs resolved elsewhere
            .Struct => hir_types.HIRType{ .Struct = 0 },
            // Map/Function/Union require payload; construct at use sites
            .Map => unreachable,
            .Enum => hir_types.HIRType{ .Enum = 0 },
            .Function => unreachable,
            .Union => unreachable,
        };
    }

    fn toHIRTypeWithNested(self: *VM, base: module.BytecodeType, nested: ?module.BytecodeType) hir_types.HIRType {
        // Currently only arrays use nested metadata from bytecode
        if (base == .Array) {
            const element_hir = if (nested) |n| toHIRType(n) else .Nothing;
            const elem_ptr = self.allocator.create(hir_types.HIRType) catch return element_hir;
            elem_ptr.* = element_hir;
            return hir_types.HIRType{ .Array = elem_ptr };
        }
        return toHIRType(base);
    }

    fn toOptionalHIRType(type_tag: ?module.BytecodeType) ?hir_types.HIRType {
        if (type_tag) |tag| {
            return toHIRType(tag);
        }
        return null;
    }

    pub fn registerCustomType(self: *VM, type_info: CustomTypeInfo) !void {
        try self.custom_type_registry.put(type_info.name, type_info);
    }

    fn registerTokenEnum(self: *VM) !void {
        // Create enum variants for Token enum
        const variants = try self.allocator.alloc(CustomTypeInfo.EnumVariant, 6);
        variants[0] = CustomTypeInfo.EnumVariant{ .name = try self.allocator.dupe(u8, "INT_LITERAL"), .index = 0 };
        variants[1] = CustomTypeInfo.EnumVariant{ .name = try self.allocator.dupe(u8, "FLOAT_LITERAL"), .index = 1 };
        variants[2] = CustomTypeInfo.EnumVariant{ .name = try self.allocator.dupe(u8, "BYTE_LITERAL"), .index = 2 };
        variants[3] = CustomTypeInfo.EnumVariant{ .name = try self.allocator.dupe(u8, "TETRA_LITERAL"), .index = 3 };
        variants[4] = CustomTypeInfo.EnumVariant{ .name = try self.allocator.dupe(u8, "STRING_LITERAL"), .index = 4 };
        variants[5] = CustomTypeInfo.EnumVariant{ .name = try self.allocator.dupe(u8, "NOTHING_LITERAL"), .index = 5 };

        const token_enum_info = CustomTypeInfo{
            .name = try self.allocator.dupe(u8, "Token"),
            .kind = .Enum,
            .enum_variants = variants,
        };

        try self.custom_type_registry.put("Token", token_enum_info);
    }

    fn initializeTokenVariable(self: *VM) !void {
        // Initialize the Token variable in slot 0 of module 0 as a string value
        if (self.module_state.len > 0) {
            const module_state = &self.module_state[0];
            const token_slot = module_state.pointer(0); // Slot 0
            const token_name = try self.allocator.dupe(u8, "Token");
            token_slot.store(HIRValue{ .string = token_name });
        }
    }

    pub fn coerceValue(self: *VM, value: HIRValue, expected_type: ?hir_types.HIRType) HIRValue {
        if (expected_type == null) {
            return value;
        }

        const target_type = expected_type.?;

        if (std.meta.eql(value, target_type)) {
            return value;
        }

        return switch (target_type) {
            .Int => switch (value) {
                .int => value,
                .byte => |b| HIRValue{ .int = @as(i64, b) },
                .float => |f| HIRValue{ .int = @as(i64, @intFromFloat(f)) },
                else => value,
            },
            .Byte => switch (value) {
                .byte => value,
                .int => |i| blk: {
                    if (i >= 0 and i <= 255) {
                        break :blk HIRValue{ .byte = @intCast(i) };
                    } else {
                        break :blk value;
                    }
                },
                .float => |f| blk: {
                    const i: i64 = @as(i64, @intFromFloat(f));
                    if (i >= 0 and i <= 255) {
                        break :blk HIRValue{ .byte = @intCast(i) };
                    } else {
                        break :blk value;
                    }
                },
                else => value,
            },
            .Float => switch (value) {
                .float => value,
                .int => |i| HIRValue{ .float = @as(f64, @floatFromInt(i)) },
                .byte => |b| HIRValue{ .float = @as(f64, @floatFromInt(b)) },
                else => value,
            },
            .String => switch (value) {
                .string => value,
                .int => |i| HIRValue{ .string = std.fmt.allocPrint(self.allocator, "{}", .{i}) catch "0" },
                .byte => |b| HIRValue{ .string = std.fmt.allocPrint(self.allocator, "0x{X:0>2}", .{b}) catch "0x00" },
                .float => |f| HIRValue{ .string = std.fmt.allocPrint(self.allocator, "{d}", .{f}) catch "0.0" },
                else => value,
            },
            .Tetra => switch (value) {
                .tetra => value,
                .int => |i| HIRValue{ .tetra = if (i != 0) 1 else 0 },
                .byte => |b| HIRValue{ .tetra = if (b != 0) 1 else 0 },
                .float => |f| HIRValue{ .tetra = if (f != 0.0) 1 else 0 },
                else => value,
            },
            else => value,
        };
    }

    pub fn valueToString(self: *VM, value: HIRValue) ![]const u8 {
        return switch (value) {
            .int => |i| try std.fmt.allocPrint(self.allocator, "{}", .{i}),
            .byte => |u| try std.fmt.allocPrint(self.allocator, "0x{X:0>2}", .{u}),
            .float => |f| {
                const rounded_down = std.math.floor(f);
                if (f - rounded_down == 0) {
                    return try std.fmt.allocPrint(self.allocator, "{d}.0", .{f});
                } else {
                    return try std.fmt.allocPrint(self.allocator, "{d}", .{f});
                }
            },
            .string => |s| try self.allocator.dupe(u8, s),
            .tetra => |t| try std.fmt.allocPrint(self.allocator, "{s}", .{switch (t) {
                0 => "false",
                1 => "true",
                2 => "both",
                3 => "neither",
                else => "invalid",
            }}),
            .nothing => try self.allocator.dupe(u8, "nothing"),
            .array => |arr| {
                var result = std.array_list.Managed(u8).init(self.allocator);
                defer result.deinit();
                try result.append('[');
                var first = true;
                for (arr.elements) |elem| {
                    if (std.meta.eql(elem, HIRValue.nothing)) break;
                    if (!first) try result.appendSlice(", ");
                    const elem_str = try self.valueToString(elem);
                    defer self.allocator.free(elem_str);
                    try result.appendSlice(elem_str);
                    first = false;
                }
                try result.append(']');
                return try result.toOwnedSlice();
            },
            .struct_instance => |s| {
                var result = std.array_list.Managed(u8).init(self.allocator);
                defer result.deinit();
                try result.appendSlice("{ ");
                var first = true;
                for (s.fields) |field| {
                    if (!first) try result.appendSlice(", ");
                    try result.appendSlice(field.name);
                    try result.appendSlice(": ");
                    const field_str = try self.valueToString(field.value);
                    defer self.allocator.free(field_str);
                    switch (field.value) {
                        .string => {
                            try result.append('"');
                            try result.appendSlice(field_str);
                            try result.append('"');
                        },
                        else => try result.appendSlice(field_str),
                    }
                    first = false;
                }
                try result.appendSlice(" }");
                return try result.toOwnedSlice();
            },
            .map => try self.allocator.dupe(u8, "{map}"),
            .enum_variant => |e| try std.fmt.allocPrint(self.allocator, ".{s}", .{e.variant_name}),
            .storage_id_ref => |storage_id| try std.fmt.allocPrint(self.allocator, "storage_id_ref({})", .{storage_id}),
        };
    }

    fn frameAllocator(self: *VM) std.mem.Allocator {
        if (self.frames.items.len == 0) return self.allocator;
        return self.frames.items[self.frames.items.len - 1].arena.allocator();
    }

    fn deepCopyValueToAllocator(self: *VM, allocator: std.mem.Allocator, value: HIRValue) !HIRValue {
        return switch (value) {
            .int, .byte, .float, .tetra, .nothing, .storage_id_ref => value,
            .string => |s| blk: {
                const duped = try allocator.dupe(u8, s);
                break :blk HIRValue{ .string = duped };
            },
            .struct_instance => |s| blk: {
                const type_name = try allocator.dupe(u8, s.type_name);
                const fields = try allocator.alloc(HIRStructField, s.fields.len);
                for (s.fields, 0..) |f, i| {
                    fields[i] = .{
                        .name = try allocator.dupe(u8, f.name),
                        .value = try self.deepCopyValueToAllocator(allocator, f.value),
                        .field_type = f.field_type,
                        .path = null,
                    };
                }
                break :blk HIRValue{ .struct_instance = .{ .type_name = type_name, .fields = fields, .field_name = null, .path = null } };
            },
            .map => |m| blk: {
                const entries = try allocator.alloc(HIRMapEntry, m.entries.len);
                for (m.entries, 0..) |e, i| {
                    entries[i] = .{
                        .key = try self.deepCopyValueToAllocator(allocator, e.key),
                        .value = try self.deepCopyValueToAllocator(allocator, e.value),
                        .path = null,
                    };
                }
                break :blk HIRValue{ .map = .{ .entries = entries, .key_type = m.key_type, .value_type = m.value_type, .path = null } };
            },
            .array => |a| blk: {
                const elems = try allocator.alloc(HIRValue, a.elements.len);
                for (a.elements, 0..) |elem, i| {
                    elems[i] = try self.deepCopyValueToAllocator(allocator, elem);
                }
                break :blk HIRValue{ .array = .{ .elements = elems, .element_type = a.element_type, .capacity = a.capacity, .path = null, .nested_element_type = a.nested_element_type } };
            },
            .enum_variant => |e| blk: {
                const type_name = try allocator.dupe(u8, e.type_name);
                const variant_name = try allocator.dupe(u8, e.variant_name);
                break :blk HIRValue{ .enum_variant = .{ .type_name = type_name, .variant_name = variant_name, .variant_index = e.variant_index, .path = null } };
            },
        };
    }

    fn execLoadModule(self: *VM, payload: anytype) VmError!void {
        const module_name = payload.module_name;

        // Find the module by name
        var module_id: ?module.ModuleId = null;
        for (self.bytecode.modules, 0..) |mod, idx| {
            if (std.mem.eql(u8, mod.name, module_name)) {
                module_id = @intCast(idx);
                break;
            }
        }

        if (module_id == null) {
            return self.reporter.reportRuntimeError(null, ErrorCode.VARIABLE_NOT_FOUND, "Module '{s}' not found", .{module_name});
        }

        const mod_id = module_id.?;
        const module_state = try self.resolveModuleState(mod_id);

        // Create a struct instance containing all the module's exported variables
        const fields = try self.allocator.alloc(HIRStructField, module_state.values.len);
        defer self.allocator.free(fields);

        for (fields, 0..) |*field, i| {
            const slot_ptr = module_state.pointer(@intCast(i));
            const value = slot_ptr.load();

            // Generate a generic variable name since we don't have the actual names
            const var_name = try std.fmt.allocPrint(self.allocator, "var_{}", .{i});
            defer self.allocator.free(var_name);

            field.* = HIRStructField{
                .name = try self.string_interner.intern(var_name),
                .value = value,
                .field_type = .Unknown,
            };
        }

        const struct_instance = HIRValue{
            .struct_instance = HIRStruct{
                .type_name = try self.string_interner.intern(module_name),
                .fields = fields,
            },
        };

        try self.stack.pushValue(struct_instance);
    }
};
