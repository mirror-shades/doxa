const std = @import("std");

pub const IRPrinter = struct {
    pub const IRPrinter = Self;
    pub const HIR = @import("../hir/soxa_types.zig");
    pub const HIRValue = @import("../hir/soxa_values.zig").HIRValue;
    pub const HIRInstruction = @import("../hir/soxa_instructions.zig").HIRInstruction;
    pub const CompareInstruction = std.meta.fieldInfo(@import("../hir/soxa_instructions.zig").HIRInstruction, .Compare).type;
    const Self = @This();
    pub const GroupTable = @import("../../common/group_table.zig").GroupTable;
    pub const EnumTable = @import("../../common/enum_table.zig").EnumTable;
    pub const StructTable = @import("../../common/struct_table.zig").StructTable;

    const Ctx = struct {
        pub const IRPrinter = Self;
        pub const HIR = @import("../hir/soxa_types.zig");
        pub const HIRValue = @import("../hir/soxa_values.zig").HIRValue;
        pub const HIRInstruction = @import("../hir/soxa_instructions.zig").HIRInstruction;
        pub const CompareInstruction = std.meta.fieldInfo(@import("../hir/soxa_instructions.zig").HIRInstruction, .Compare).type;
        pub const PeekEmitState = Self.PeekEmitState;
        pub const PeekStringInfo = Self.PeekStringInfo;
        pub const StackType = Self.StackType;
        pub const StackVal = Self.StackVal;
        pub const Region = Self.Region;
        pub const IntRange = @import("./ir_printer/int_range.zig").IntRange;
        pub const SignFacts = @import("./ir_printer/int_range.zig").SignFacts;
        pub const VariableInfo = Self.VariableInfo;
        pub const StackIncoming = Self.StackIncoming;
        pub const StackSlot = Self.StackSlot;
        pub const StackMergeState = Self.StackMergeState;
        pub const EnumVariantMeta = Self.EnumVariantMeta;
        pub const escapeLLVMString = Self.escapeLLVMString;
        pub const internPeekString = Self.internPeekString;
        pub const OverflowBehavior = Self.OverflowBehavior;
        pub const GroupTable = Self.GroupTable;
        pub const EnumTable = Self.EnumTable;
        pub const StructTable = Self.StructTable;

        /// Function attributes every emitted `define` references. Clang's C
        /// frontend emits `"tune-cpu"="generic"` when only the CPU model is
        /// given (`-mcpu=native` selects features, not `-mtune`), so the `.ll`
        /// backend path must name it too: otherwise the function inherits the
        /// native tune model, whose loop unroller picks a pathological unroll
        /// for some serial loops. Naming it keeps the two sides tuned alike.
        pub const function_attr_group = " #0";
        pub const function_attr_block = "\nattributes #0 = { \"tune-cpu\"=\"generic\" }\n";
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
    pub const callDiverges = CoreMethods.callDiverges;
    pub const functionSymbol = CoreMethods.functionSymbol;
    pub const mangleGlobalName = CoreMethods.mangleGlobalName;
    pub const cloneHeapForStore = CoreMethods.cloneHeapForStore;
    pub const cloneHeapForSnapshot = CoreMethods.cloneHeapForSnapshot;
    pub const cloneHeapForReturn = CoreMethods.cloneHeapForReturn;
    pub const cloneHeapForGlobalStore = CoreMethods.cloneHeapForGlobalStore;
    pub const cloneHeapForAliasStore = CoreMethods.cloneHeapForAliasStore;
    pub const cloneHeapValue = CoreMethods.cloneHeapValue;
    pub const currentRegionTag = CoreMethods.currentRegionTag;
    pub const callerLevels = CoreMethods.callerLevels;
    pub const rehomeTypeEligible = CoreMethods.rehomeTypeEligible;
    pub const rehomeUnknownToClone = CoreMethods.rehomeUnknownToClone;
    pub const plainStoreProven = CoreMethods.plainStoreProven;
    pub const plainGlobalStoreProven = CoreMethods.plainGlobalStoreProven;
    pub const recordVarRegion = CoreMethods.recordVarRegion;
    pub const recordVarRange = CoreMethods.recordVarRange;
    pub const varRange = CoreMethods.varRange;
    pub const prepareLoopRanges = CoreMethods.prepareLoopRanges;
    pub const computeCallResultRange = CoreMethods.computeCallResultRange;
    pub const rehomeForLocalStore = CoreMethods.rehomeForLocalStore;
    pub const rehomeForGlobalStore = CoreMethods.rehomeForGlobalStore;

    const ModuleLayoutMethods = @import("./ir_printer/module_layout.zig").Methods(Ctx);
    const SharedHandlerMethods = @import("./ir_printer/shared_handlers.zig").Methods(Ctx);
    pub const writeModule = ModuleLayoutMethods.writeModule;
    pub const writeMainProgram = ModuleLayoutMethods.writeMainProgram;
    pub const computeDescriptorSkips = ModuleLayoutMethods.computeDescriptorSkips;
    pub const registerStructTableLayouts = ModuleLayoutMethods.registerStructTableLayouts;
    pub const structFieldsAllScalar = ModuleLayoutMethods.structFieldsAllScalar;
    pub const reflectedContains = ModuleLayoutMethods.reflectedContains;
    pub const markTypeNeeds = ModuleLayoutMethods.markTypeNeeds;
    pub const markNestedStructs = ModuleLayoutMethods.markNestedStructs;
    pub const markArrayElementNeeds = ModuleLayoutMethods.markArrayElementNeeds;
    pub const skippedStructWords = ModuleLayoutMethods.skippedStructWords;
    pub const markInstructionNeeds = ModuleLayoutMethods.markInstructionNeeds;
    pub const findFunctionsSectionStart = ModuleLayoutMethods.findFunctionsSectionStart;
    pub const findTopLevelInitEnd = ModuleLayoutMethods.findTopLevelInitEnd;
    pub const getFunctionRange = ModuleLayoutMethods.getFunctionRange;
    pub const collectFunctionStructReturnInfo = ModuleLayoutMethods.collectFunctionStructReturnInfo;

    const FunctionEmitMethods = @import("./ir_printer/function_emit.zig").Methods(Ctx);
    pub const writeFunction = FunctionEmitMethods.writeFunction;
    pub const nextTemp = FunctionEmitMethods.nextTemp;
    pub const nextTempText = FunctionEmitMethods.nextTempText;
    pub const buildEnumPrintMap = FunctionEmitMethods.buildEnumPrintMap;
    pub const emitEnumPrint = FunctionEmitMethods.emitEnumPrint;
    pub const emitQuantifierWrappers = FunctionEmitMethods.emitQuantifierWrappers;

    const VerifyMethods = @import("./ir_printer/verify.zig").Methods(Ctx);
    pub const verifyEnter = VerifyMethods.verifyEnter;
    pub const hirFault = VerifyMethods.hirFault;
    pub const collectLiveJumpTargets = VerifyMethods.collectLiveJumpTargets;
    pub const requireStack = VerifyMethods.requireStack;
    pub const requireRepr = VerifyMethods.requireRepr;
    pub const requireReprIn = VerifyMethods.requireReprIn;
    pub const verifyStore = VerifyMethods.verifyStore;

    const ValueHelperMethods = @import("./ir_printer/value_helpers.zig").Methods(Ctx);
    pub const createEnumTypeNameGlobal = ValueHelperMethods.createEnumTypeNameGlobal;
    pub const emitRTCallReturningString = ValueHelperMethods.emitRTCallReturningString;
    pub const strOutSlots = ValueHelperMethods.strOutSlots;
    pub const callReturningString = ValueHelperMethods.callReturningString;
    pub const pushStringResult = ValueHelperMethods.pushStringResult;
    pub const boxDoxaValue = ValueHelperMethods.boxDoxaValue;
    pub const doxaValueSlot = ValueHelperMethods.doxaValueSlot;
    pub const enumTypeNameFor = ValueHelperMethods.enumTypeNameFor;
    pub const ensurePointer = ValueHelperMethods.ensurePointer;
    pub const ensureI64 = ValueHelperMethods.ensureI64;
    pub const unwrapDoxaValueToType = ValueHelperMethods.unwrapDoxaValueToType;
    pub const boxMember = ValueHelperMethods.boxMember;
    pub const boxMemberCount = ValueHelperMethods.boxMemberCount;
    pub const boxHeader = ValueHelperMethods.boxHeader;
    pub const findMemberIndex = ValueHelperMethods.findMemberIndex;
    pub const isBoxedMemberType = ValueHelperMethods.isBoxedMemberType;
    pub const buildDoxaValue = ValueHelperMethods.buildDoxaValue;
    pub const arrayElementSize = ValueHelperMethods.arrayElementSize;
    pub const arrayElementTag = ValueHelperMethods.arrayElementTag;
    pub const convertValueToArrayStorage = ValueHelperMethods.convertValueToArrayStorage;
    pub const convertArrayStorageToValue = ValueHelperMethods.convertArrayStorageToValue;
    pub const ensureString = ValueHelperMethods.ensureString;
    pub const loadArrayLength = ValueHelperMethods.loadArrayLength;
    pub const fixedArrayInnermostLLVMType = ValueHelperMethods.fixedArrayInnermostLLVMType;
    pub const buildFixedArrayLLVMTypeStr = ValueHelperMethods.buildFixedArrayLLVMTypeStr;
    pub const fixedArrayLevelLLVMType = ValueHelperMethods.fixedArrayLevelLLVMType;

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
    pub const emitArrayConcat = CollectionsEmitMethods.emitArrayConcat;
    pub const emitArrayLen = CollectionsEmitMethods.emitArrayLen;
    pub const emitFlatArrayPeek = CollectionsEmitMethods.emitFlatArrayPeek;
    pub const emitSyntheticArrayHeader = CollectionsEmitMethods.emitSyntheticArrayHeader;
    pub const isFlatStructArray = CollectionsEmitMethods.isFlatStructArray;
    pub const emitMap = CollectionsEmitMethods.emitMap;
    pub const emitMapGet = CollectionsEmitMethods.emitMapGet;
    pub const emitMapSet = CollectionsEmitMethods.emitMapSet;
    pub const ensureBool = CollectionsEmitMethods.ensureBool;
    pub const emitCompareInstruction = CollectionsEmitMethods.emitCompareInstruction;

    const SharedHandlers = SharedHandlerMethods;
    pub const handleConst = SharedHandlers.handleConst;
    pub const handleDup = SharedHandlers.handleDup;
    pub const handlePop = SharedHandlers.handlePop;
    pub const handleSwap = SharedHandlers.handleSwap;
    pub const handleArith = SharedHandlers.handleArith;
    pub const handleCompare = SharedHandlers.handleCompare;
    pub const handleLogicalOp = SharedHandlers.handleLogicalOp;
    pub const handleConvert = SharedHandlers.handleConvert;
    pub const handleTypeCheck = SharedHandlers.handleTypeCheck;
    pub const handleStringOp = SharedHandlers.handleStringOp;
    pub const enumToStringArgs = SharedHandlers.enumToStringArgs;
    pub const emitBoxedToString = SharedHandlers.emitBoxedToString;
    pub const writeStringValue = SharedHandlers.writeStringValue;
    pub const handlePeek = SharedHandlers.handlePeek;
    pub const handlePeekStruct = SharedHandlers.handlePeekStruct;
    pub const handleMemberCheck = SharedHandlers.handleMemberCheck;
    pub const handleUnboxPayload = SharedHandlers.handleUnboxPayload;
    pub const handleBox = SharedHandlers.handleBox;
    pub const handleUnbox = SharedHandlers.handleUnbox;
    pub const handleAssertFail = SharedHandlers.handleAssertFail;
    pub const handleCall = SharedHandlers.handleCall;
pub const emitFallibleZigCall = SharedHandlers.emitFallibleZigCall;
    pub const handleStoreDeclGlobal = SharedHandlers.handleStoreDeclGlobal;
    pub const handleStoreVarGlobal = SharedHandlers.handleStoreVarGlobal;
    pub const handleLoadVarGlobal = SharedHandlers.handleLoadVarGlobal;
    pub const handlePushStorageIdGlobal = SharedHandlers.handlePushStorageIdGlobal;
    pub const wrapFixedArrayHeader = SharedHandlers.wrapFixedArrayHeader;

    const StructsEnumsEmitMethods = @import("./ir_printer/structs_enums_emit.zig").Methods(Ctx);
    pub const emitStructNew = StructsEnumsEmitMethods.emitStructNew;
    pub const emitGetField = StructsEnumsEmitMethods.emitGetField;
    pub const emitSetField = StructsEnumsEmitMethods.emitSetField;
    pub const canBoxFixedArrayField = StructsEnumsEmitMethods.canBoxFixedArrayField;
    pub const boxFixedArrayField = StructsEnumsEmitMethods.boxFixedArrayField;
    pub const placeFieldBox = StructsEnumsEmitMethods.placeFieldBox;
    pub const structFieldDescTag = StructsEnumsEmitMethods.structFieldDescTag;
    pub const boxFixedStructArrayField = StructsEnumsEmitMethods.boxFixedStructArrayField;
    pub const fixedStructElementInfo = StructsEnumsEmitMethods.fixedStructElementInfo;
    pub const emitFixedStructArrayHeader = StructsEnumsEmitMethods.emitFixedStructArrayHeader;
    pub const buildNonOwningHeader = StructsEnumsEmitMethods.buildNonOwningHeader;
    pub const wrapFixedStructArrayHeader = StructsEnumsEmitMethods.wrapFixedStructArrayHeader;
    pub const emitPeekInstruction = StructsEnumsEmitMethods.emitPeekInstruction;
    pub const hydrateStructMetadata = StructsEnumsEmitMethods.hydrateStructMetadata;
    pub const resolveStructFieldNames = StructsEnumsEmitMethods.resolveStructFieldNames;
    pub const findLabelIndex = StructsEnumsEmitMethods.findLabelIndex;
    pub const buildI64StructType = StructsEnumsEmitMethods.buildI64StructType;
    pub const storeStructStringField = StructsEnumsEmitMethods.storeStructStringField;
    pub const loadStructStringField = StructsEnumsEmitMethods.loadStructStringField;
    pub const getOrCreateStructDescGlobal = StructsEnumsEmitMethods.getOrCreateStructDescGlobal;
    pub const getOrCreateStructDescGlobalByName = StructsEnumsEmitMethods.getOrCreateStructDescGlobalByName;
    pub const getOrCreateEnumDescGlobal = StructsEnumsEmitMethods.getOrCreateEnumDescGlobal;
    pub const emitBoxRegistry = StructsEnumsEmitMethods.emitBoxRegistry;
    pub const emitBoxRegistryInit = StructsEnumsEmitMethods.emitBoxRegistryInit;
    pub const emitEnumInitCalls = StructsEnumsEmitMethods.emitEnumInitCalls;
    pub const hirTypeToStackType = StructsEnumsEmitMethods.hirTypeToStackType;
    pub const hirTypeToTypeString = StructsEnumsEmitMethods.hirTypeToTypeString;
    pub const stackTypeToLLVMType = StructsEnumsEmitMethods.stackTypeToLLVMType;
    pub const hirTypeToLLVMType = StructsEnumsEmitMethods.hirTypeToLLVMType;

    allocator: std.mem.Allocator,
    io: std.Io,
    /// Each inline-Zig callee's wrapper parameter types, from the program.
    zig_fn_param_types: std.StringHashMap([]const HIR.HIRType),
    peek_string_counter: usize,
    string_pool_len: usize = 0,

    global_types: std.StringHashMap(StackType),
    global_array_types: std.StringHashMap(HIR.HIRType),
    global_enum_types: std.StringHashMap([]const u8),
    global_struct_field_types: std.StringHashMap([]HIR.HIRType),
    global_struct_field_names: std.StringHashMap([]const []const u8),
    global_struct_type_names: std.StringHashMap([]const u8),
    global_fixed_array_info: std.StringHashMap(GlobalFixedArrayInfo),
    /// The group or union a global was declared as, when it holds a
    /// `%DoxaValue` box. A later store into it carries the *member* being
    /// stored, so only the declaration can say which member index to re-pack.
    /// Written only by `handleStoreDeclGlobal`.
    global_boxed_types: std.StringHashMap(HIR.HIRType),
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
    /// Every union and group type the program boxes, numbered in one id
    /// space: the box id in a `%DoxaValue`'s `reserved` word and the index of
    /// its entry in the runtime box registry (`emitBoxRegistry`).
    box_ids: std.AutoHashMapUnmanaged(BoxKey, u32) = .empty,
    boxed_types: std.ArrayListUnmanaged(HIR.HIRType) = .empty,
    /// The analyzer's type tables: canonical keys, layouts, and members.
    group_table: *const GroupTable,
    enum_table: *const EnumTable,
    struct_table: *const StructTable,
    entry_str_out_ptr: ?[]const u8 = null,
    entry_str_out_len: ?[]const u8 = null,
    /// Alloca lines discovered while emitting the current function/program body
    /// that must live in the entry block. Emitting an `alloca` inside a loop
    /// makes it a *dynamic* alloca, which leaks shadow-stack space on every
    /// iteration; these are hoisted to entry and replayed before the body.
    entry_allocas: std.array_list.Managed([]const u8),
    /// Distinguishes the named registers of hoisted synthetic ArrayHeaders
    /// within one function. Reset at each function/program entry.
    synth_header_counter: usize = 0,
    in_function_context: bool = false,
    scope_depth: usize = 0,
    /// Set while emitting a function whose scope arenas are provably unused.
    /// Suppresses every `doxa_scope_enter` / `exit` / `reset` in that body so a
    /// scalar leaf function does not pay two page-allocator round trips per call.
    scopes_elided: bool = false,
    exited_scopes: std.AutoHashMap(u32, void),
    /// Region class of each local variable's heap payload (A1). Populated while
    /// a function body is emitted so a `LoadVar` knows whether the object it
    /// loads provably outlives a later rehome store's destination. Global scope
    /// kinds never appear here; globals are always `Root`.
    var_regions: std.AutoHashMap(HIR.Slot, Region),
    /// Phase D: value range of each local variable's integer payload. Same
    /// shape and lifetime as `var_regions` — a per-function map consulted by
    /// `LoadVar` so an arithmetic lowering can trust a bound across a variable
    /// reference. The recorded range is the hull of every store seen in the
    /// variable's own basic block; a store from any other block widens it to
    /// the whole of `i64` (see `recordVarRange`).
    var_ranges: std.AutoHashMap(HIR.Slot, IntRange),
    /// Phase D: the basic block each entry of `var_ranges` was recorded in, so
    /// a store from a different block can be recognised as one this linear walk
    /// cannot reason about.
    var_range_blocks: std.AutoHashMap(HIR.Slot, []const u8),
    /// Phase D: the basic block currently being emitted. Mirrors the
    /// `current_block` the emitter threads through its own helpers.
    current_block: []const u8 = "entry",
    /// Phase D-1: how integer `add`/`sub`/`mul` handle signed overflow. Chosen
    /// once from the compile mode (trap in checked modes, wrap in fast ones) and
    /// consulted by the arithmetic lowering. `Saturate` is a valid member of the
    /// policy but no mode selects it yet.
    arith_overflow: OverflowBehavior = .Wrap,
    /// Phase D-1 follow-on: the program-wide label/function index used by the
    /// loop-carried and interprocedural range analysis. Built once before
    /// emission; `null` until then.
    range_ctx: ?@import("./ir_printer/range_flow.zig").Context = null,
    /// Phase D-1 follow-on: loop-head variable ranges, keyed by `loop_start_*`
    /// label. The emitter activates the entry matching the loop it is emitting.
    loop_head_envs: std.StringHashMap(std.AutoHashMap(HIR.Slot, IntRange)),
    /// The loop-head range map currently in scope, or `null` outside a loop.
    /// Consulted by `varRange` before the block-local walk.
    active_loop_range: ?*const std.AutoHashMap(HIR.Slot, IntRange) = null,
    /// B2: struct type names that reach a reflection site anywhere in the program
    /// (borrowed from the generator). Such a type must keep its descriptor.
    reflected_structs: ?*const std.StringHashMap(void) = null,
    /// B2: set when a group/unknown reflection target makes per-type reasoning
    /// impossible; then no struct may skip its descriptor.
    force_struct_descriptors: bool = false,
    /// B2: scalar-only struct type names proven never to need the descriptor
    /// (non-reflected and never crossing a signature/container/global boundary).
    /// Computed once before emission; `emitStructNew` skips the registry write
    /// for these and clones use the typed scalar path.
    skip_descriptor_structs: std.StringHashMap(void),
    /// Structs that are an element of a one-dimensional dynamic array. A skipped
    /// one is rehomed by owner lookup (`doxa_struct_rehome_scalar_*`) so an
    /// `each` binding still writes through to the element; any other skipped
    /// struct has no aliased element and is simply copied.
    array_element_structs: std.AutoHashMap(HIR.StructId, void),
    /// The peek accumulator active for the current emit pass. A container
    /// element descriptor has to be materialized from `emitSetField`, which
    /// carries no `PeekEmitState` parameter, so the pass installs its state
    /// here and restores the previous value on exit.
    active_peek_state: ?*PeekEmitState = null,

    /// Where an internal-error diagnostic is sent. Set by the driver; null in
    /// isolated emitter tests, which fall back to stderr.
    reporter: ?*@import("../../utils/reporting.zig").Reporter = null,
    /// The instruction being emitted, for `hirFault` (Phase A verifier).
    verify_function: []const u8 = "<top level>",
    verify_index: usize = 0,
    verify_tag: []const u8 = "",

    pub const EnumVariantMeta = struct {
        index: u32,
        name: []const u8,
    };

    /// Maximum plausible length for an operand / variable name.  Names longer
    /// than this are assumed to be corrupted by an upstream pipeline bug and
    /// are handled defensively (e.g. by emitting a safe fallback).
    pub const MAX_SANE_NAME_LEN: usize = 1000;

    pub const StackType = enum { I64, F64, I8, I1, I2, PTR, STRING, Value, Nothing };

    /// Phase D value-range lattice, re-exported from the emitter module that
    /// owns it so `StackVal` and the per-function map can name it.
    pub const IntRange = Ctx.IntRange;

    /// Phase D-1 integer-overflow policy. Re-exported from the HIR so the
    /// emitter and its callers agree on one type.
    pub const OverflowBehavior = @import("../hir/soxa_instructions.zig").OverflowBehavior;

    /// Static region class of a heap value's allocating arena, relative to the
    /// function being emitted (A1 region analysis). A value outlives any store
    /// destination inside the function exactly when its arena is the function's
    /// own body scope or an ancestor of it (`Func` / `Root`); values born in a
    /// reusable loop scope (`Deep`) die at the next iteration reset. `Unknown`
    /// means the analysis could not prove a class, and the emitter must keep the
    /// conservative runtime rehome call.
    pub const Region = enum {
        Root,
        Func,
        Deep,
        /// A3: a value constructed directly in a `return` and allocated in the
        /// caller's arena at construction. It outlives the callee body, so the
        /// return clone is skipped; it is not necessarily resident in the frame
        /// that consumes the call, so store decisions stay conservative.
        Caller,
        Unknown,
    };

    /// A box type's compile-time identity: a union id and a group id may be
    /// equal, so the kind is part of it.
    pub const BoxKey = struct {
        kind: enum { Union, Group },
        id: u32,
    };

    pub const StackVal = struct {
        name: []const u8,
        ty: StackType,
        region: Region = .Unknown,
        /// Phase D: what is statically known about this value's magnitude.
        /// The default is the whole of `i64`, so a producer that does not opt
        /// in simply forgoes the cheaper arithmetic lowerings.
        int_range: IntRange = .unknown(),
        array_type: ?HIR.HIRType = null,
        enum_type_name: ?[]const u8 = null,
        struct_field_types: ?[]HIR.HIRType = null,
        struct_field_names: ?[]const []const u8 = null,
        struct_type_name: ?[]const u8 = null,
        string_literal_value: ?[]const u8 = null,
        fixed_array_depth: u3 = 0,
        fixed_array_sizes: [4]u32 = [_]u32{0} ** 4,
        /// The boxed type a `.Value` was built for (a union or group). Carried
        /// beside the aggregate so re-boxing it into a different union can
        /// re-pack the member index from the source type instead of silently
        /// keeping the first box's index.
        boxed_type: ?HIR.HIRType = null,
        /// For a value pushed by `PushStorageId`: how many alias re-passes lie
        /// between the frame that owns the storage and the frame taking the
        /// alias. 0 is a variable owned by the immediate caller; each re-pass
        /// of an existing alias adds one. A heap store through the alias
        /// re-homes into the owning frame's arena at that depth.
        alias_extra: u8 = 0,
        /// True when this value was read through an alias (`^` parameter or a
        /// method receiver's `this`). A heap field stored through it must be
        /// cloned into the owning frame's arena, not the current scope, so it
        /// survives the current frame's exit.
        alias_owned: bool = false,
        /// Runtime alias depth for an alias-owned value: the LLVM value (an
        /// `i64`) naming how many frames separate this frame from the frame
        /// that owns the aliased storage. 0 means the immediate caller. This is
        /// carried across calls as a trailing argument so a store through an
        /// alias re-homes into the true owner's arena, not a soon-to-be-freed
        /// intermediate callee.
        alias_depth_value: ?[]const u8 = null,
    };

    pub const VariableInfo = struct {
        ptr_name: []const u8,
        stack_type: StackType,
        /// The group or union the slot was declared as, when it holds a
        /// `%DoxaValue` box: the box type a load of it pushes.
        boxed_declared_type: ?HIR.HIRType = null,
        array_type: ?HIR.HIRType = null,
        enum_type_name: ?[]const u8 = null,
        struct_field_types: ?[]HIR.HIRType = null,
        struct_field_names: ?[]const []const u8 = null,
        struct_type_name: ?[]const u8 = null,
        fixed_array_depth: u3 = 0,
        fixed_array_sizes: [4]u32 = [_]u32{0} ** 4,
    };

    pub const GlobalFixedArrayInfo = struct {
        depth: u3,
        sizes: [4]u32,
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
                slots[i] = .empty;
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
        len_name: []const u8,
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
        var buffer = std.ArrayListUnmanaged(u8).empty;
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

        const len_name = try std.fmt.allocPrint(allocator, "@.peek.str.len.{d}", .{next_id.*});
        errdefer allocator.free(len_name);
        next_id.* += 1;

        const len_line = try std.fmt.allocPrint(
            allocator,
            "{s} = constant i64 {d}\n",
            .{ len_name, value.len },
        );
        errdefer allocator.free(len_line);
        try globals.append(len_line);

        const info = PeekStringInfo{
            .name = global_name,
            .len_name = len_name,
            .length = value.len,
        };
        try strings.append(info);
        try map.put(key_copy, strings.items.len - 1);

        return info;
    }
};
