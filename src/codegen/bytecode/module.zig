const std = @import("std");
const hir_types = @import("../hir/soxa_types.zig");
const hir_instructions = @import("../hir/soxa_instructions.zig");
const hir_values = @import("../hir/soxa_values.zig");
const Reporting = @import("../../utils/reporting.zig");

pub const SpecVersion: u16 = 1;

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
    Any,
};

pub fn typeFromHIR(hir_type: hir_types.HIRType) BytecodeType {
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
        else => .Any,
    };
}

pub const ConstantValue = hir_values.HIRValue;

pub const CallTarget = struct {
    function_index: u32,
    qualified_name: []const u8,
    call_kind: hir_types.CallKind,
    target_module: ?[]const u8,
    target_module_id: ?ModuleId,
};

pub const Instruction = union(enum) {
    PushConst: struct { constant_index: ConstIndex },
    Dup,
    Pop,
    Swap,
    LoadSlot: SlotOperand,
    StoreSlot: struct { target: SlotOperand, type_tag: BytecodeType },
    StoreConstSlot: SlotOperand,
    PushStorageRef: SlotOperand,
    BindAlias: struct { alias_slot: SlotIndex, type_tag: BytecodeType },
    LoadAlias: struct { slot_index: SlotIndex },
    Arith: struct { op: hir_instructions.ArithOp, type_tag: BytecodeType },
    Convert: struct { from: BytecodeType, to: BytecodeType },
    Compare: struct { op: hir_instructions.CompareOp, type_tag: BytecodeType },
    LogicalOp: struct { op: hir_instructions.LogicalOpType },
    StringOp: struct { op: hir_instructions.StringOpType },
    Jump: struct { label_id: LabelId },
    JumpIfFalse: struct { label_id: LabelId, condition_type: BytecodeType },
    JumpIfTrue: struct { label_id: LabelId, condition_type: BytecodeType },
    Label: struct { id: LabelId },
    Call: struct { target: CallTarget, arg_count: u32, return_type: BytecodeType },
    TailCall: struct { target: CallTarget, arg_count: u32, return_type: BytecodeType },
    Return: struct { has_value: bool, return_type: BytecodeType },
    TypeOf: struct { value_type: BytecodeType },
    TypeCheck: struct { type_name: []const u8 },
    ArrayNew: struct { element_type: BytecodeType, static_size: u32, nested_element_type: ?BytecodeType },
    ArrayGet: struct { bounds_check: bool },
    ArraySet: struct { bounds_check: bool },
    ArrayPush: struct { resize: hir_instructions.ResizeBehavior },
    ArrayPop,
    ArrayInsert,
    ArrayRemove,
    ArraySlice,
    ArrayLen,
    ArrayConcat,
    Range: struct { element_type: BytecodeType },
    Exists: struct { predicate_type: BytecodeType },
    Forall: struct { predicate_type: BytecodeType },
    StructNew: struct { type_name: []const u8, field_count: u32, field_types: []const hir_types.HIRType, size_bytes: u32 },
    EnumNew: struct { enum_name: []const u8, variant_name: []const u8, variant_index: u32 },
    GetField: struct { field_name: []const u8, container_type: BytecodeType, field_index: u32 },
    SetField: struct { field_name: []const u8, container_type: BytecodeType, field_index: u32 },
    StoreFieldName: struct { field_name: []const u8 },
    TryBegin: struct { label_id: LabelId },
    TryCatch: struct { exception_type: ?BytecodeType },
    Throw: struct { exception_type: BytecodeType },
    EnterScope: struct { scope_id: u32, var_count: u32 },
    ExitScope: struct { scope_id: u32 },
    Print,
    PrintInterpolated: struct {
        format_parts: []const []const u8,
        placeholder_indices: []const u32,
        argument_count: u32,
        format_part_ids: []const u32,
    },
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
    PrintStruct: struct {
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
    },
    MapGet: struct { key_type: BytecodeType },
    MapSet: struct { key_type: BytecodeType },
    AssertFail: struct {
        location: Reporting.Location,
        has_message: bool,
    },
    Halt,
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
    }
};
