const std = @import("std");
const hir_types = @import("../hir/soxa_types.zig");
const hir_instructions = @import("../hir/soxa_instructions.zig");
const hir_values = @import("../hir/soxa_values.zig");
const Reporting = @import("../../utils/reporting.zig");

pub const SpecVersion: u16 = 1;

pub const BuiltinId = enum(u8) {
    length,
    push,
    pop,
    insert,
    remove,
    clear,
    find,
    slice,
    string,
    int,
    float,
    byte,
    type_,
    print,
    println,
    assert,
    panic,
    exit,
    std,
    power,
    powi,
    dice_roll,
    range,
    exists_quantifier_gt,
    exists_quantifier_eq,
    forall_quantifier_gt,
    forall_quantifier_eq,
};

pub const ModuleId = u16;
pub const InvalidModuleId: ModuleId = std.math.maxInt(ModuleId);
pub const SlotIndex = u32;
pub const LabelId = u32;
pub const ConstIndex = u32;
pub const StringId = u32;

pub const SlotKind = enum(u3) {
    local,
    module_global,
    imported_module,
    builtin,
    alias,
};

pub const SlotOperand = struct {
    slot: SlotIndex,
    kind: SlotKind,
    module_id: ?ModuleId = null,
};

pub const BytecodeType = enum(u8) {
    Int,
    Byte,
    Float,
    String,
    Tetra,
    Nothing,
    Array,
    Struct,
    Map,
    Enum,
    Function,
    Union,
};

pub fn typeFromHIR(hir_type: hir_types.HIRType) !BytecodeType {
    return switch (hir_type) {
        .Int => .Int,
        .Byte => .Byte,
        .Float => .Float,
        .String => .String,
        .Tetra => .Tetra,
        .Nothing => .Nothing,
        .Array => .Array,
        .Struct => .Struct,
        .Map => .Map,
        .Enum => .Enum,
        .Group => .Enum,
        .Function => .Function,
        .Union => .Union,
        .Unknown => {
            std.debug.print("Unknown type: {}\n", .{hir_type});
            return error.UnknownType;
        },
        .Poison => {
            std.debug.print("Poison type: {}\n", .{hir_type});
            return error.PoisonType;
        },
    };
}

pub const ConstantValue = hir_values.HIRValue;

pub const CallTarget = struct {
    function_index: u32,
    qualified_name: []const u8,
    call_kind: hir_types.CallKind,
    target_module: ?[]const u8,
    target_module_id: ?ModuleId,
    builtin_id: ?BuiltinId = null,
};

pub const Instruction = union(enum) {
    PushConst: struct { constant_index: ConstIndex },
    Dup,
    Pop,
    Swap,
    LoadSlot: SlotOperand,
    StoreSlot: struct { target: SlotOperand, type_tag: BytecodeType },
    PushStorageRef: SlotOperand,
    BindAlias: struct { alias_slot: SlotIndex, type_tag: BytecodeType },
    LoadAlias: struct { slot_index: SlotIndex },
    StoreAlias: struct { slot_index: SlotIndex, type_tag: BytecodeType },
    Arith: struct { op: hir_instructions.ArithOp, type_tag: BytecodeType },
    Convert: struct { from: BytecodeType, to: BytecodeType },
    Compare: struct { op: hir_instructions.CompareOp, type_tag: BytecodeType },
    LogicalOp: struct { op: hir_instructions.LogicalOpType },
    StringOp: struct { op: hir_instructions.StringOpType },
    Jump: struct { label_id: LabelId },
    JumpIfFalse: struct { label_id: LabelId, condition_type: BytecodeType },
    JumpIfTrue: struct { label_id: LabelId, condition_type: BytecodeType },
    Label: struct { id: LabelId },
    Call: struct { target: CallTarget, arg_count: u32, return_type: BytecodeType, tail: bool = false },
    Return: struct { has_value: bool, return_type: BytecodeType },
    TypeCheck: struct { type_name: []const u8 },
    GroupCheck: struct { member_index: u32 },
    GroupExtractPayload: struct {},
    ArrayNew: struct {
        element_type: BytecodeType,
        static_size: u32,
        nested_element_type: ?BytecodeType,
        storage_kind: hir_types.ArrayStorageKind,
        nested_sizes: [4]u32 = [_]u32{0} ** 4,
        nested_depth: u3 = 0,
    },
    ArrayGet: struct { bounds_check: bool },
    ArraySet: struct { bounds_check: bool },
    ArrayPush: struct { resize: hir_instructions.ResizeBehavior },
    ArrayPop,
    ArrayInsert,
    ArrayRemove,
    ArraySlice,
    ArrayLen,
    ArrayConcat,
    ArrayCompoundAssign: struct { bounds_check: bool, op: hir_instructions.ArithOp },
    StructNew: struct {
        type_name: []const u8,
        field_count: u32,
        field_names: []const []const u8,
        field_types: []const hir_types.HIRType,
        size_bytes: u32,
    },
    GetField: struct { field_name: []const u8, container_type: BytecodeType, field_index: u32 },
    SetField: struct { field_name: []const u8, container_type: BytecodeType, field_index: u32 },
    StoreFieldName: struct { field_name: []const u8 },
    EnterScope: struct { scope_id: u32, var_count: u32 },
    ExitScope: struct { scope_id: u32 },
    Peek: struct {
        name: ?[]const u8,
        value_type: BytecodeType,
        location: ?Reporting.Location,
        union_members: ?[][]const u8,
    },
    PeekStruct: struct {
        type_name: []const u8,
        field_count: u32,
        field_names: []const []const u8,
        field_types: []const hir_types.HIRType,
        location: ?Reporting.Location,
        should_pop_after_peek: bool,
    },
    Map: struct {
        entries: []const hir_values.HIRMapEntry,
        key_type: BytecodeType,
        value_type: BytecodeType,
        has_else_value: bool,
    },
    MapGet: struct { key_type: BytecodeType },
    MapSet: struct { key_type: BytecodeType },
    AssertFail: struct {
        location: Reporting.Location,
        has_message: bool,
    },
    Unreachable: struct {
        location: Reporting.Location,
    },
    Halt,
    LoadModule: struct {
        module_name: []const u8,
        field_names: []const []const u8 = &[_][]const u8{},
        field_slots: []const u32 = &[_]u32{},
    },
    Nop,
};

pub const BytecodeFunction = struct {
    name: []const u8,
    qualified_name: []const u8,
    module_id: ModuleId,
    arity: u32,
    return_type: BytecodeType,
    start_label: []const u8,
    body_label: ?[]const u8,
    start_ip: u32,
    body_ip: ?u32,
    local_var_count: u32,
    is_entry: bool,
    param_types: []BytecodeType,
    param_is_alias: []bool,
};

pub const ModuleDescriptor = struct {
    id: ModuleId,
    name: []const u8,
    global_var_count: u32,
};

pub const BytecodeModule = struct {
    allocator: std.mem.Allocator,
    spec_version: u16 = SpecVersion,
    constants: []const ConstantValue,
    string_pool: []const []const u8,
    instructions: []Instruction,
    functions: []BytecodeFunction,
    modules: []ModuleDescriptor,
    artifact_path: ?[]u8 = null,
    source_path: ?[]const u8 = null,

    /// Caller is responsible for ensuring the backing HIR program remains
    /// alive for the lifetime of the bytecode module. We only free the data
    /// allocated during lowering (instruction buffers and per-function slices).
    pub fn deinit(self: *BytecodeModule) void {
        for (self.functions) |func| {
            self.allocator.free(func.param_types);
            self.allocator.free(func.param_is_alias);
        }
        self.allocator.free(self.functions);
        self.allocator.free(self.instructions);
        self.allocator.free(self.modules);
        if (self.artifact_path) |path| {
            self.allocator.free(path);
            self.artifact_path = null;
        }
        if (self.source_path) |path| {
            self.allocator.free(path);
            self.source_path = null;
        }
    }
};
