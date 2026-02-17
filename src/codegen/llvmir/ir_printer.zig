const std = @import("std");

pub const IRPrinter = struct {
    pub const IRPrinter = Self;
    pub const HIR = @import("../hir/soxa_types.zig");
    pub const HIRValue = @import("../hir/soxa_values.zig").HIRValue;
    pub const HIRInstruction = @import("../hir/soxa_instructions.zig").HIRInstruction;
    pub const CompareInstruction = std.meta.TagPayload(@import("../hir/soxa_instructions.zig").HIRInstruction, .Compare);
    const Self = @This();

    const Ctx = struct {
        pub const IRPrinter = Self;
        pub const HIR = @import("../hir/soxa_types.zig");
        pub const HIRValue = @import("../hir/soxa_values.zig").HIRValue;
        pub const HIRInstruction = @import("../hir/soxa_instructions.zig").HIRInstruction;
        pub const CompareInstruction = std.meta.TagPayload(@import("../hir/soxa_instructions.zig").HIRInstruction, .Compare);
        pub const PeekEmitState = Self.PeekEmitState;
        pub const PeekStringInfo = Self.PeekStringInfo;
        pub const StackType = Self.StackType;
        pub const StackVal = Self.StackVal;
        pub const VariableInfo = Self.VariableInfo;
        pub const StackIncoming = Self.StackIncoming;
        pub const StackSlot = Self.StackSlot;
        pub const StackMergeState = Self.StackMergeState;
        pub const EnumVariantMeta = Self.EnumVariantMeta;
        pub const escapeLLVMString = Self.escapeLLVMString;
        pub const internPeekString = Self.internPeekString;
    };

    const CoreMethods = @import("./ir_printer/core.zig").Methods(Ctx);
    pub const init = CoreMethods.init;
    pub const deinit = CoreMethods.deinit;
    pub const emitToFile = CoreMethods.emitToFile;
    pub const formatFloatLiteral = CoreMethods.formatFloatLiteral;
    pub const paramTypeMatchesStack = CoreMethods.paramTypeMatchesStack;
    pub const coerceForMerge = CoreMethods.coerceForMerge;
    pub const coerceForStore = CoreMethods.coerceForStore;
    pub const recordStackForLabel = CoreMethods.recordStackForLabel;
    pub const restoreStackForLabel = CoreMethods.restoreStackForLabel;
    pub const mapBuiltinToRuntime = CoreMethods.mapBuiltinToRuntime;
    pub const mangleGlobalName = CoreMethods.mangleGlobalName;

    const ModuleLayoutMethods = @import("./ir_printer/module_layout.zig").Methods(Ctx);
    pub const writeModule = ModuleLayoutMethods.writeModule;
    pub const writeMainProgram = ModuleLayoutMethods.writeMainProgram;
    pub const findFunctionsSectionStart = ModuleLayoutMethods.findFunctionsSectionStart;
    pub const getFunctionRange = ModuleLayoutMethods.getFunctionRange;
    pub const collectFunctionStructReturnInfo = ModuleLayoutMethods.collectFunctionStructReturnInfo;
    pub const collectStructFieldNames = ModuleLayoutMethods.collectStructFieldNames;

    const FunctionEmitMethods = @import("./ir_printer/function_emit.zig").Methods(Ctx);
    pub const writeFunction = FunctionEmitMethods.writeFunction;
    pub const nextTemp = FunctionEmitMethods.nextTemp;
    pub const nextTempText = FunctionEmitMethods.nextTempText;
    pub const buildEnumPrintMap = FunctionEmitMethods.buildEnumPrintMap;
    pub const emitEnumPrint = FunctionEmitMethods.emitEnumPrint;
    pub const emitQuantifierWrappers = FunctionEmitMethods.emitQuantifierWrappers;

    const ValueHelperMethods = @import("./ir_printer/value_helpers.zig").Methods(Ctx);
    pub const createEnumTypeNameGlobal = ValueHelperMethods.createEnumTypeNameGlobal;
    pub const ensurePointer = ValueHelperMethods.ensurePointer;
    pub const ensureI64 = ValueHelperMethods.ensureI64;
    pub const unwrapDoxaValueToType = ValueHelperMethods.unwrapDoxaValueToType;
    pub const findUnionMemberIndex = ValueHelperMethods.findUnionMemberIndex;
    pub const buildDoxaValue = ValueHelperMethods.buildDoxaValue;
    pub const arrayElementSize = ValueHelperMethods.arrayElementSize;
    pub const arrayElementTag = ValueHelperMethods.arrayElementTag;
    pub const convertValueToArrayStorage = ValueHelperMethods.convertValueToArrayStorage;
    pub const convertArrayStorageToValue = ValueHelperMethods.convertArrayStorageToValue;
    pub const loadArrayLength = ValueHelperMethods.loadArrayLength;

    const CollectionsEmitMethods = @import("./ir_printer/collections_emit.zig").Methods(Ctx);
    pub const emitArrayNew = CollectionsEmitMethods.emitArrayNew;
    pub const emitArrayGet = CollectionsEmitMethods.emitArrayGet;
    pub const emitArraySet = CollectionsEmitMethods.emitArraySet;
    pub const emitArrayGetAndArith = CollectionsEmitMethods.emitArrayGetAndArith;
    pub const emitArrayPush = CollectionsEmitMethods.emitArrayPush;
    pub const emitArrayPop = CollectionsEmitMethods.emitArrayPop;
    pub const emitArrayInsert = CollectionsEmitMethods.emitArrayInsert;
    pub const emitArrayRemove = CollectionsEmitMethods.emitArrayRemove;
    pub const emitArraySlice = CollectionsEmitMethods.emitArraySlice;
    pub const emitArrayLen = CollectionsEmitMethods.emitArrayLen;
    pub const emitArrayConcat = CollectionsEmitMethods.emitArrayConcat;
    pub const emitMap = CollectionsEmitMethods.emitMap;
    pub const emitMapGet = CollectionsEmitMethods.emitMapGet;
    pub const emitMapSet = CollectionsEmitMethods.emitMapSet;
    pub const emitRange = CollectionsEmitMethods.emitRange;
    pub const ensureBool = CollectionsEmitMethods.ensureBool;
    pub const emitCompareInstruction = CollectionsEmitMethods.emitCompareInstruction;

    const StructsEnumsEmitMethods = @import("./ir_printer/structs_enums_emit.zig").Methods(Ctx);
    pub const emitStructNew = StructsEnumsEmitMethods.emitStructNew;
    pub const emitGetField = StructsEnumsEmitMethods.emitGetField;
    pub const emitSetField = StructsEnumsEmitMethods.emitSetField;
    pub const emitPeekInstruction = StructsEnumsEmitMethods.emitPeekInstruction;
    pub const emitStructValuePeek = StructsEnumsEmitMethods.emitStructValuePeek;
    pub const hydrateStructMetadata = StructsEnumsEmitMethods.hydrateStructMetadata;
    pub const resolveStructFieldNames = StructsEnumsEmitMethods.resolveStructFieldNames;
    pub const findLabelIndex = StructsEnumsEmitMethods.findLabelIndex;
    pub const buildFallbackStructType = StructsEnumsEmitMethods.buildFallbackStructType;
    pub const buildI64StructType = StructsEnumsEmitMethods.buildI64StructType;
    pub const getOrCreateStructDescGlobal = StructsEnumsEmitMethods.getOrCreateStructDescGlobal;
    pub const getOrCreateEnumDescGlobal = StructsEnumsEmitMethods.getOrCreateEnumDescGlobal;
    pub const emitEnumInitCalls = StructsEnumsEmitMethods.emitEnumInitCalls;
    pub const hirTypeToStackType = StructsEnumsEmitMethods.hirTypeToStackType;
    pub const hirTypeToTypeString = StructsEnumsEmitMethods.hirTypeToTypeString;
    pub const stackTypeToLLVMType = StructsEnumsEmitMethods.stackTypeToLLVMType;
    pub const hirTypeToLLVMType = StructsEnumsEmitMethods.hirTypeToLLVMType;

    allocator: std.mem.Allocator,
    peek_string_counter: usize,

    global_types: std.StringHashMap(StackType),
    global_array_types: std.StringHashMap(HIR.HIRType),
    global_enum_types: std.StringHashMap([]const u8),
    global_struct_field_types: std.StringHashMap([]HIR.HIRType),
    global_struct_field_names: std.StringHashMap([]const []const u8),
    global_struct_type_names: std.StringHashMap([]const u8),
    defined_globals: std.StringHashMap(bool),
    struct_fields_by_id: std.AutoHashMap(HIR.StructId, []HIR.HIRType),
    struct_type_names_by_id: std.AutoHashMap(HIR.StructId, []const u8),
    struct_field_names_by_type: std.StringHashMap([]const []const u8),
    struct_field_enum_type_names_by_type: std.StringHashMap([]const ?[]const u8),
    struct_desc_globals_by_type: std.StringHashMap([]const u8),
    function_struct_return_fields: std.StringHashMap([]HIR.HIRType),
    function_struct_return_type_names: std.StringHashMap([]const u8),
    enum_desc_globals_by_type: std.StringHashMap([]const u8),
    last_emitted_enum_value: ?u64 = null,
    enum_print_map: std.StringHashMap(std.ArrayListUnmanaged(EnumVariantMeta)),

    pub const EnumVariantMeta = struct {
        index: u32,
        name: []const u8,
    };

    /// Maximum plausible length for an operand / variable name.  Names longer
    /// than this are assumed to be corrupted by an upstream pipeline bug and
    /// are handled defensively (e.g. by emitting a safe fallback).
    pub const MAX_SANE_NAME_LEN: usize = 1000;

    pub const StackType = enum { I64, F64, I8, I1, I2, PTR, Value, Nothing };

    pub const StackVal = struct {
        name: []const u8,
        ty: StackType,
        array_type: ?HIR.HIRType = null,
        enum_type_name: ?[]const u8 = null,
        struct_field_types: ?[]HIR.HIRType = null,
        struct_field_names: ?[]const []const u8 = null,
        struct_type_name: ?[]const u8 = null,
        string_literal_value: ?[]const u8 = null,
    };

    pub const VariableInfo = struct {
        ptr_name: []const u8,
        stack_type: StackType,
        array_type: ?HIR.HIRType = null,
        enum_type_name: ?[]const u8 = null,
        struct_field_types: ?[]HIR.HIRType = null,
        struct_field_names: ?[]const []const u8 = null,
        struct_type_name: ?[]const u8 = null,
    };

    pub const StackIncoming = struct {
        block: []const u8,
        value: StackVal,
    };

    pub const StackSlot = std.ArrayListUnmanaged(StackIncoming);

    pub const StackMergeState = struct {
        slots: []StackSlot,

        pub fn init(allocator: std.mem.Allocator, slot_count: usize) !StackMergeState {
            var slots = try allocator.alloc(StackSlot, slot_count);
            var i: usize = 0;
            while (i < slot_count) : (i += 1) {
                slots[i] = StackSlot{};
            }
            return .{ .slots = slots };
        }

        pub fn deinit(self: *StackMergeState, allocator: std.mem.Allocator) void {
            for (self.slots) |*slot| slot.deinit(allocator);
            allocator.free(self.slots);
        }
    };

    pub const PeekStringInfo = struct {
        name: []const u8,
        length: usize,
    };

    pub const PeekEmitState = struct {
        allocator: std.mem.Allocator,
        globals: std.array_list.Managed([]const u8),
        string_map: std.StringHashMap(usize),
        strings: std.array_list.Managed(PeekStringInfo),
        next_id_ptr: *usize,

        pub fn init(allocator: std.mem.Allocator, next_id_ptr: *usize) PeekEmitState {
            return .{
                .allocator = allocator,
                .globals = std.array_list.Managed([]const u8).init(allocator),
                .string_map = std.StringHashMap(usize).init(allocator),
                .strings = std.array_list.Managed(PeekStringInfo).init(allocator),
                .next_id_ptr = next_id_ptr,
            };
        }

        pub fn deinit(self: *PeekEmitState) void {
            for (self.globals.items) |g| self.allocator.free(g);
            self.globals.deinit();

            var it = self.string_map.iterator();
            while (it.next()) |entry| {
                self.allocator.free(entry.key_ptr.*);
            }
            self.string_map.deinit();

            for (self.strings.items) |info| self.allocator.free(info.name);
            self.strings.deinit();
        }
    };

    pub fn escapeLLVMString(allocator: std.mem.Allocator, text: []const u8) ![]u8 {
        var buffer = std.ArrayListUnmanaged(u8){};
        defer buffer.deinit(allocator);
        const hex = "0123456789ABCDEF";
        for (text) |ch| {
            if (ch >= 32 and ch <= 126 and ch != '"' and ch != '\\') {
                try buffer.append(allocator, ch);
            } else {
                try buffer.append(allocator, '\\');
                try buffer.append(allocator, hex[(ch >> 4) & 0xF]);
                try buffer.append(allocator, hex[ch & 0xF]);
            }
        }
        return buffer.toOwnedSlice(allocator);
    }

    pub fn internPeekString(
        allocator: std.mem.Allocator,
        map: *std.StringHashMap(usize),
        strings: *std.array_list.Managed(PeekStringInfo),
        next_id: *usize,
        globals: *std.array_list.Managed([]const u8),
        value: []const u8,
    ) !PeekStringInfo {
        if (map.get(value)) |idx| {
            return strings.items[idx];
        }

        const key_copy = try allocator.dupe(u8, value);
        errdefer allocator.free(key_copy);

        const escaped = try escapeLLVMString(allocator, value);
        defer allocator.free(escaped);

        const global_name = try std.fmt.allocPrint(allocator, "@.peek.str.{d}", .{next_id.*});
        errdefer allocator.free(global_name);
        next_id.* += 1;

        const global_line = try std.fmt.allocPrint(
            allocator,
            "{s} = private unnamed_addr constant [{d} x i8] c\"{s}\\00\"\n",
            .{ global_name, value.len + 1, escaped },
        );
        errdefer allocator.free(global_line);

        try globals.append(global_line);

        const info = PeekStringInfo{
            .name = global_name,
            .length = value.len + 1,
        };
        try strings.append(info);
        try map.put(key_copy, strings.items.len - 1);

        return info;
    }
};
