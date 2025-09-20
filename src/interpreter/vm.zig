const std = @import("std");
const hir_soxa = @import("../codegen/hir/soxa.zig");
const hir_instructions = @import("../codegen/hir/soxa_instructions.zig");
const hir_values = @import("../codegen/hir/soxa_values.zig");
const hir_types = @import("../codegen/hir/soxa_types.zig");
const HIRInstruction = hir_instructions.HIRInstruction;
// Implicit defer callback type
const DeferAction = *const fn () void;
const HIRValue = hir_values.HIRValue;
const HIRArray = hir_values.HIRArray;
const HIRMap = hir_values.HIRMap;
const HIRMapEntry = hir_values.HIRMapEntry;
const HIRStruct = hir_values.HIRStruct;
const HIRStructField = hir_values.HIRStructField;
const HIRProgram = hir_types.HIRProgram;
const HIRType = hir_types.HIRType;
const HIRFunction = HIRProgram.HIRFunction;
const instructions = @import("instructions.zig");
const Reporting = @import("../utils/reporting.zig");
const Location = Reporting.Location;
const Reporter = Reporting.Reporter;
const Errors = @import("../utils/errors.zig");
const ErrorList = Errors.ErrorList;
const ErrorCode = Errors.ErrorCode;
const memory = @import("../utils/memory.zig");
const MemoryManager = memory.MemoryManager;
const ScopeManager = memory.ScopeManager;
const Scope = memory.Scope;
const Variable = memory.Variable;
const TokenType = @import("../types/token.zig").TokenType;
const TokenLiteral = @import("../types/types.zig").TokenLiteral;
const TypeInfo = @import("../ast/ast.zig").TypeInfo;
const StructField = @import("../types/types.zig").StructField;
const StringInterner = memory.StringInterner;
const CustomTypeInfo = memory.CustomTypeInfo;
const types = @import("../types/types.zig");
const core = @import("core.zig");
const HIRStack = core.HIRStack;
const CallStack = core.CallStack;
const HotVar = core.HotVar;
const HIRFrame = core.HIRFrame;
const CallFrame = core.CallFrame;
const ops_logical = @import("ops/logical.zig");
const ops_strings = @import("ops/strings.zig");
const ops_array = @import("ops/array.zig");
const ops_arith = @import("ops/arith.zig");
const ops_compare = @import("ops/compare.zig");
const ops_control = @import("ops/control.zig");
const ops_stack = @import("ops/stack.zig");
const ops_type = @import("ops/type.zig");
const decl_variables = @import("decl/variables.zig");
const ops_functions = @import("calls/functions.zig");
const debug_print = @import("calls/print.zig");

const STACK_SIZE: u32 = 1024 * 1024;

/// HIR-based Virtual Machine - executes HIR instructions directly
const CanonicalTypes = struct {
    int: *TypeInfo,
    byte: *TypeInfo,
    float: *TypeInfo,
    string: *TypeInfo,
    tetra: *TypeInfo,
    nothing: *TypeInfo,
};

const ProfileEntry = struct { count: u64 = 0, ns: u64 = 0 };

pub const HIRVM = struct {
    program: *HIRProgram,
    reporter: *Reporter,
    memory_manager: *MemoryManager, // Use existing memory manager from main
    allocator: std.mem.Allocator,

    // Execution state
    ip: u32 = 0, // Instruction pointer (index into instructions array)
    stack: HIRStack,
    call_stack: CallStack, // Track function calls and returns
    current_scope: *Scope, // Current execution scope
    running: bool = true,

    // Label resolution
    label_map: std.StringHashMap(u32), // label_name -> instruction index

    // Performance optimizations
    var_cache: std.StringHashMap(u32), // Cache var_name → storage_id (invalidated on scope changes)
    turbo_mode: bool = true, // Ultra-aggressive optimizations for pure computational loops

    // Hot variable cache - direct storage for most accessed variables
    hot_vars: [4]?HotVar = [_]?HotVar{null} ** 4,
    hot_var_count: u8 = 0,

    // Profiling support
    profile_mode: bool = false,
    in_function_depth: u32 = 0,
    profile_map: std.StringHashMap(ProfileEntry),

    // Tail call optimization: flag to skip scope creation on next EnterScope
    skip_next_enter_scope: bool = false,

    // Reserved for future fast-path hints
    _reserved_flags: u1 = 0,

    // New field for catch target
    current_catch_target: ?u32 = null, // Track the current catch block's IP

    // Add string interner to VM
    string_interner: *StringInterner,

    // NEW: Custom type registry for runtime type safety
    custom_type_registry: std.StringHashMap(CustomTypeInfo),
    // Canonical TypeInfo singletons for primitive types to avoid per-store allocations
    canonical_types: CanonicalTypes,

    // Implicit defer support: per-scope stacks of zero-arg callbacks to run on scope exit
    defer_stacks: std.array_list.Managed(std.array_list.Managed(DeferAction)),

    pub fn tokenLiteralToHIRValueWithType(self: *HIRVM, token_literal: TokenLiteral, type_info: *TypeInfo) HIRValue {
        // Handle enum types with proper type information
        if (type_info.base == .Enum and token_literal == .enum_variant) {
            const variant_name = token_literal.enum_variant;
            const enum_type_name = type_info.custom_type orelse "unknown";

            // Look up the variant index from the custom type registry
            var variant_index: u32 = 0;
            if (self.custom_type_registry.get(enum_type_name)) |custom_type| {
                if (custom_type.kind == .Enum) {
                    if (custom_type.enum_variants) |variants| {
                        for (variants, 0..) |variant, i| {
                            if (std.mem.eql(u8, variant.name, variant_name)) {
                                variant_index = @intCast(i);
                                break;
                            }
                        }
                    }
                }
            }

            return HIRValue{ .enum_variant = .{
                .type_name = enum_type_name,
                .variant_name = variant_name,
                .variant_index = variant_index,
                .path = null,
            } };
        }

        // Preserve array element types using provided TypeInfo (fix for empty arrays)
        if (token_literal == .array and type_info.base == .Array) {
            const hir_val = self.tokenLiteralToHIRValue(token_literal);
            switch (hir_val) {
                .array => |arr| {
                    var mutable_arr = arr;
                    if (type_info.array_type) |elem_info| {
                        const mapped_elem_type: HIRType = switch (elem_info.base) {
                            .Int => .Int,
                            .Byte => .Byte,
                            .Float => .Float,
                            .String => .String,
                            .Tetra => .Tetra,
                            .Array => .Array,
                            .Struct => .Struct,
                            .Enum => .Enum,
                            else => mutable_arr.element_type,
                        };
                        mutable_arr.element_type = mapped_elem_type;
                    }
                    return HIRValue{ .array = mutable_arr };
                },
                else => {},
            }
        }

        // Otherwise, use the regular conversion
        return self.tokenLiteralToHIRValue(token_literal);
    }

    pub fn tokenLiteralToHIRValue(self: *HIRVM, token_literal: TokenLiteral) HIRValue {
        return switch (token_literal) {
            .int => |i| HIRValue{ .int = i },
            .byte => |u| HIRValue{ .byte = u },
            .float => |f| HIRValue{ .float = f },
            .string => |s| HIRValue{ .string = s },
            .tetra => |t| HIRValue{ .tetra = switch (t) {
                .false => 0,
                .true => 1,
                .both => 2,
                .neither => 3,
            } },
            .nothing => HIRValue.nothing,
            .array => |arr| blk: {
                // Convert TokenLiteral array to HIRArray
                var hir_elements = self.allocator.alloc(HIRValue, arr.len) catch break :blk HIRValue.nothing;
                var element_type: HIRType = .Unknown;

                for (arr, 0..) |element, i| {
                    hir_elements[i] = self.tokenLiteralToHIRValue(element);

                    // Infer element type from first element
                    if (i == 0 and element_type == .Unknown) {
                        element_type = switch (hir_elements[i]) {
                            .int => .Int,
                            .byte => .Byte,
                            .float => .Float,
                            .string => .String,
                            .tetra => .Tetra,
                            .nothing => .Nothing,
                            .array => .Array,
                            .struct_instance => .Struct,
                            else => .Unknown,
                        };
                    }
                }

                break :blk HIRValue{ .array = .{
                    .elements = hir_elements,
                    .element_type = element_type,
                    .capacity = @intCast(arr.len),
                } };
            },
            .struct_value => |s| blk: {
                // Convert StructField array to HIRStructField array
                var hir_fields = self.allocator.alloc(HIRStructField, s.fields.len) catch break :blk HIRValue.nothing;
                for (s.fields, 0..) |field, i| {
                    // Recursively convert field value, but preserve enum type names when possible
                    var field_value_hir = self.tokenLiteralToHIRValue(field.value);

                    // If this field is an enum variant literal, try to recover the enum type from registry
                    if (field.value == .enum_variant) {
                        if (self.getCustomType(s.type_name)) |struct_info| {
                            if (struct_info.kind == .Struct) {
                                if (struct_info.struct_fields) |sf| {
                                    // Find matching field by name and check if it declares a custom enum type
                                    for (sf) |decl_field| {
                                        if (std.mem.eql(u8, decl_field.name, field.name)) {
                                            if (decl_field.custom_type_name) |enum_type_name| {
                                                // Rebuild enum HIR with correct type name
                                                field_value_hir = HIRValue{ .enum_variant = .{
                                                    .type_name = enum_type_name,
                                                    .variant_name = field.value.enum_variant,
                                                    .variant_index = struct_info.getEnumVariantIndex(field.value.enum_variant) orelse 0,
                                                    .path = null,
                                                } };
                                            }
                                            break;
                                        }
                                    }
                                }
                            }
                        }
                    }

                    hir_fields[i] = HIRStructField{
                        .name = field.name,
                        .value = field_value_hir,
                        // Infer field type from the value
                        .field_type = switch (field_value_hir) {
                            .int => .Int,
                            .byte => .Byte,
                            .float => .Float,
                            .string => .String,
                            .tetra => .Tetra,
                            .nothing => .Nothing,
                            .struct_instance => .Struct,
                            .enum_variant => .Enum,
                            .array => .Array,
                            else => .Unknown,
                        },
                        .path = s.path,
                    };
                }
                break :blk HIRValue{ .struct_instance = .{
                    .type_name = s.type_name,
                    .fields = hir_fields,
                    .field_name = null,
                    .path = s.path,
                } };
            },
            .map => |m| blk: {
                // Convert TokenLiteral map (string-keyed) back to HIRMap.
                // Heuristic: if a key parses as an integer, treat it as Int; otherwise keep as String.
                var hir_entries = self.allocator.alloc(HIRMapEntry, m.count()) catch break :blk HIRValue.nothing;
                var i: usize = 0;
                var iter = m.iterator();
                while (iter.next()) |entry| {
                    const key_str = entry.key_ptr.*;
                    const maybe_int = std.fmt.parseInt(i64, key_str, 10) catch null;
                    const key_hir: HIRValue = if (maybe_int) |ival|
                        HIRValue{ .int = ival }
                    else
                        HIRValue{ .string = key_str };

                    hir_entries[i] = HIRMapEntry{
                        .key = key_hir,
                        .value = self.tokenLiteralToHIRValue(entry.value_ptr.*),
                    };
                    i += 1;
                }
                break :blk HIRValue{ .map = .{
                    .entries = hir_entries,
                    .key_type = .Unknown,
                    .value_type = .Unknown,
                    .path = null,
                } };
            },
            .enum_variant => |variant_name| HIRValue{
                .enum_variant = .{
                    .type_name = "unknown", // We'll need to handle this better
                    .variant_name = variant_name,
                    .variant_index = 0, // We'll need to handle this better
                    .path = null,
                },
            },
            else => HIRValue.nothing,
        };
    }

    pub fn hirValueToTokenLiteral(self: *HIRVM, hir_value: HIRValue) TokenLiteral {
        return switch (hir_value) {
            .int => |i| TokenLiteral{ .int = i },
            .byte => |u| TokenLiteral{ .byte = u },
            .float => |f| TokenLiteral{ .float = f },
            .string => |s| TokenLiteral{ .string = s },
            .tetra => |t| TokenLiteral{ .tetra = switch (t) {
                0 => .false,
                1 => .true,
                2 => .both,
                3 => .neither,
                else => .false,
            } },
            .nothing => TokenLiteral{ .nothing = {} },
            .array => |arr| blk: {
                var token_elements = self.allocator.alloc(TokenLiteral, arr.elements.len) catch {
                    break :blk TokenLiteral{ .nothing = {} };
                };
                for (arr.elements, 0..) |element, i| {
                    token_elements[i] = self.hirValueToTokenLiteral(element);
                }
                break :blk TokenLiteral{ .array = token_elements };
            },
            .struct_instance => |s| blk: {
                // Convert HIRStructField array to StructField array
                var token_fields = self.allocator.alloc(StructField, s.fields.len) catch break :blk TokenLiteral{ .nothing = {} };
                for (s.fields, 0..) |field, i| {
                    // Recursively convert field value
                    token_fields[i] = StructField{
                        .name = field.name,
                        .value = self.hirValueToTokenLiteral(field.value),
                    };
                }
                break :blk TokenLiteral{ .struct_value = .{
                    .type_name = s.type_name,
                    .fields = token_fields,
                    .path = s.path,
                } };
            },
            .map => |m| blk: {
                // Convert HIRMap to TokenLiteral map
                var token_map = std.StringHashMap(TokenLiteral).init(self.allocator);
                for (m.entries) |entry| {
                    // Convert the key to a string for HashMap key
                    const key_str = switch (entry.key) {
                        .string => |s| s,
                        .int => |i| std.fmt.allocPrint(self.allocator, "{d}", .{i}) catch break :blk TokenLiteral{ .nothing = {} },
                        else => break :blk TokenLiteral{ .nothing = {} }, // Only string and int keys supported
                    };
                    const value_token = self.hirValueToTokenLiteral(entry.value);
                    token_map.put(key_str, value_token) catch break :blk TokenLiteral{ .nothing = {} };
                }
                break :blk TokenLiteral{ .map = token_map };
            },
            .enum_variant => |e| TokenLiteral{ .enum_variant = e.variant_name },
            .storage_id_ref => |storage_id| {
                // For storage_id_ref, we need to look up the actual value
                if (self.memory_manager.scope_manager.value_storage.get(storage_id)) |storage| {
                    return self.hirValueToTokenLiteral(self.tokenLiteralToHIRValue(storage.value));
                } else {
                    return TokenLiteral{ .nothing = {} };
                }
            },
        };
    }

    pub fn hirValueToTokenType(self: *HIRVM, hir_value: HIRValue) TokenType {
        return switch (hir_value) {
            .int => .INT,
            .byte => .BYTE,
            .float => .FLOAT,
            .string => .STRING,
            .tetra => .TETRA,
            .nothing => .NOTHING,
            // Complex types - map to closest TokenType
            .array => .ARRAY,
            .struct_instance => .IDENTIFIER, // Structs represented as identifiers
            .map => .ARRAY, // Maps similar to arrays
            .enum_variant => .IDENTIFIER, // Enums represented as identifiers
            .storage_id_ref => |storage_id| {
                // For storage_id_ref, we need to look up the actual type
                if (self.memory_manager.scope_manager.value_storage.get(storage_id)) |storage| {
                    return storage.type;
                } else {
                    return .NOTHING;
                }
            },
        };
    }

    pub fn hirValueToTypeInfo(self: *HIRVM, hir_value: HIRValue) TypeInfo {
        return switch (hir_value) {
            .int => TypeInfo{ .base = .Int, .is_mutable = true },
            .byte => TypeInfo{ .base = .Byte, .is_mutable = true },
            .float => TypeInfo{ .base = .Float, .is_mutable = true },
            .string => TypeInfo{ .base = .String, .is_mutable = true },
            .tetra => TypeInfo{ .base = .Tetra, .is_mutable = true },
            .nothing => TypeInfo{ .base = .Nothing, .is_mutable = true },
            // Complex types - map to appropriate TypeInfo
            .array => |arr| blk: {
                // Allocate element TypeInfo to preserve array element type
                const elem_info = self.allocator.create(TypeInfo) catch {
                    break :blk TypeInfo{ .base = .Array, .is_mutable = true };
                };
                elem_info.* = switch (arr.element_type) {
                    .Int => TypeInfo{ .base = .Int },
                    .Byte => TypeInfo{ .base = .Byte },
                    .Float => TypeInfo{ .base = .Float },
                    .String => TypeInfo{ .base = .String },
                    .Tetra => TypeInfo{ .base = .Tetra },
                    .Array => TypeInfo{ .base = .Array },
                    .Struct => TypeInfo{ .base = .Struct },
                    .Map => TypeInfo{ .base = .Map },
                    .Enum => TypeInfo{ .base = .Enum },
                    else => TypeInfo{ .base = .Nothing },
                };
                break :blk TypeInfo{ .base = .Array, .array_type = elem_info, .is_mutable = true };
            },
            .struct_instance => TypeInfo{ .base = .Struct, .is_mutable = true }, // Struct types need resolution
            .map => TypeInfo{ .base = .Map, .is_mutable = true },
            .enum_variant => |e| TypeInfo{ .base = .Enum, .is_mutable = true, .custom_type = e.type_name }, // Preserve enum type name
            .storage_id_ref => |storage_id| {
                // For storage_id_ref, we need to look up the actual type info
                if (self.memory_manager.scope_manager.value_storage.get(storage_id)) |storage| {
                    return storage.type_info.*;
                } else {
                    return TypeInfo{ .base = .Nothing, .is_mutable = true };
                }
            },
        };
    }

    pub fn init(program: *HIRProgram, reporter: *Reporter, memory_manager: *MemoryManager) !HIRVM {
        const allocator = memory_manager.getAllocator();

        // Ensure a fresh root scope for each run to avoid leftover globals from prior executions
        if (memory_manager.scope_manager.root_scope) |existing_root| {
            existing_root.deinit();
            memory_manager.scope_manager.root_scope = null;
        }

        // Create a brand new root scope
        const fresh_root = try memory_manager.scope_manager.createScope(null, memory_manager);
        memory_manager.scope_manager.root_scope = fresh_root;

        // Create a new execution scope for this HIR VM run (child of fresh root)
        // This allows proper cleanup and isolation
        const execution_scope = try memory_manager.scope_manager.createScope(memory_manager.scope_manager.root_scope, memory_manager);

        const string_interner = try allocator.create(StringInterner);
        string_interner.* = StringInterner.init(allocator);

        var vm = HIRVM{
            .program = program,
            .reporter = reporter,
            .memory_manager = memory_manager,
            .allocator = allocator, // Use the passed allocator (memory manager's arena)
            .stack = try HIRStack.init(allocator), // Use the passed allocator
            .call_stack = try CallStack.init(allocator), // Use the passed allocator
            .current_scope = execution_scope,
            .label_map = std.StringHashMap(u32).init(allocator), // Use the passed allocator
            .var_cache = std.StringHashMap(u32).init(allocator), // Use the passed allocator
            .turbo_mode = true,
            .hot_vars = [_]?HotVar{null} ** 4,
            .hot_var_count = 0,
            .profile_mode = false,
            .in_function_depth = 0,
            .profile_map = std.StringHashMap(ProfileEntry).init(allocator),
            .string_interner = string_interner,
            .custom_type_registry = std.StringHashMap(CustomTypeInfo).init(allocator), // NEW: Initialize custom type registry
            .canonical_types = undefined,
            ._reserved_flags = 0,
            .defer_stacks = std.array_list.Managed(std.array_list.Managed(DeferAction)).init(allocator),
        };

        // Enable profiling if environment variable DOXA_PROFILE is set
        if (std.process.getEnvVarOwned(allocator, "DOXA_PROFILE")) |val| {
            vm.profile_mode = val.len > 0 and val[0] != '0';
            allocator.free(val);
        } else |_| {}

        // Pre-resolve all labels for efficient jumps
        try vm.resolveLabels();

        // Initialize canonical TypeInfo singletons
        vm.canonical_types.int = try allocator.create(TypeInfo);
        vm.canonical_types.int.* = TypeInfo{ .base = .Int, .is_mutable = true };
        vm.canonical_types.byte = try allocator.create(TypeInfo);
        vm.canonical_types.byte.* = TypeInfo{ .base = .Byte, .is_mutable = true };
        vm.canonical_types.float = try allocator.create(TypeInfo);
        vm.canonical_types.float.* = TypeInfo{ .base = .Float, .is_mutable = true };
        vm.canonical_types.string = try allocator.create(TypeInfo);
        vm.canonical_types.string.* = TypeInfo{ .base = .String, .is_mutable = true };
        vm.canonical_types.tetra = try allocator.create(TypeInfo);
        vm.canonical_types.tetra.* = TypeInfo{ .base = .Tetra, .is_mutable = true };
        vm.canonical_types.nothing = try allocator.create(TypeInfo);
        vm.canonical_types.nothing.* = TypeInfo{ .base = .Nothing, .is_mutable = true };

        return vm;
    }

    pub fn deinit(self: *HIRVM) void {
        // Aggressively unwind and deinitialize all active scopes up to (but not including) the root scope.
        // This ensures we don't leak per-scope arena allocations when execution halts early (e.g., AssertFail).
        var scope: ?*Scope = self.current_scope;
        const root_scope = self.memory_manager.scope_manager.root_scope;
        while (scope) |s| {
            if (root_scope != null and s == root_scope.?) break;
            const parent = s.parent;
            s.deinit();
            scope = parent;
        }
        if (root_scope) |rs| {
            self.current_scope = rs; // Normalize to root for consistency
        }

        // Clean up string interner
        self.string_interner.deinit();
        self.allocator.destroy(self.string_interner);

        // Clean up custom type registry
        self.custom_type_registry.deinit();

        // Destroy canonical TypeInfos
        self.allocator.destroy(self.canonical_types.int);
        self.allocator.destroy(self.canonical_types.byte);
        self.allocator.destroy(self.canonical_types.float);
        self.allocator.destroy(self.canonical_types.string);
        self.allocator.destroy(self.canonical_types.tetra);
        self.allocator.destroy(self.canonical_types.nothing);

        // Clean up VM data structures
        self.stack.deinit();
        self.call_stack.deinit();
        self.label_map.deinit();
        self.var_cache.deinit();
        self.profile_map.deinit();

        // Deinit defer stacks
        var i: usize = self.defer_stacks.items.len;
        while (i > 0) : (i -= 1) self.defer_stacks.items[i - 1].deinit();
        self.defer_stacks.deinit();
    }

    /// Get a canonical TypeInfo pointer for primitive types to avoid allocations
    pub fn getCanonicalTypeInfo(self: *HIRVM, ti: TypeInfo) !*TypeInfo {
        return switch (ti.base) {
            .Int => self.canonical_types.int,
            .Byte => self.canonical_types.byte,
            .Float => self.canonical_types.float,
            .String => self.canonical_types.string,
            .Tetra => self.canonical_types.tetra,
            .Nothing => self.canonical_types.nothing,
            else => blk: {
                // For complex types, allocate a copy (caller owns arena lifetime)
                const ptr = try self.allocator.create(TypeInfo);
                ptr.* = ti;
                break :blk ptr;
            },
        };
    }

    /// NEW: Register a custom type for runtime type safety
    pub fn registerCustomType(self: *HIRVM, type_info: CustomTypeInfo) !void {
        try self.custom_type_registry.put(type_info.name, type_info);
    }

    /// NEW: Get a custom type by name
    pub fn getCustomType(self: *HIRVM, type_name: []const u8) ?CustomTypeInfo {
        return self.custom_type_registry.get(type_name);
    }

    /// Pre-resolve all labels to instruction indices for O(1) jump lookup
    fn resolveLabels(self: *HIRVM) !void {
        for (self.program.instructions, 0..) |instruction, i| {
            switch (instruction) {
                .Label => |label| {
                    // Labels should point to the next instruction (the one to execute when jumping to the label)
                    const target_ip = if (i + 1 < self.program.instructions.len) i + 1 else i;
                    try self.label_map.put(label.name, @intCast(target_ip));
                },
                else => {},
            }
        }

        // Cache function entry/body IPs for fast calls
        for (self.program.function_table) |*fn_info| {
            if (self.label_map.get(fn_info.start_label)) |start_ip| {
                fn_info.start_ip = start_ip;
            }
            if (fn_info.body_label) |bl| {
                if (self.label_map.get(bl)) |body_ip| {
                    fn_info.body_ip = body_ip;
                }
            }
        }
    }

    /// Main execution loop - directly execute HIR instructions
    pub fn run(self: *HIRVM) !?HIRFrame {

        // sanity check on soxa code
        try self.validateHIRProgram();

        while (self.running and self.ip < self.program.instructions.len) {
            const instruction = self.program.instructions[self.ip];

            var start_ns: i128 = 0;
            if (self.profile_mode and self.in_function_depth > 0) {
                start_ns = std.time.nanoTimestamp();
            }

            // Check for interrupt every 1000 instructions to allow graceful shutdown
            if (self.ip % 1000 == 0) {
                // This is a simple way to allow the OS to handle interrupts
                // The program will exit naturally when interrupted
            }

            try self.executeInstruction(instruction);

            if (self.profile_mode and self.in_function_depth > 0 and instruction != .Call) {
                const elapsed: u64 = @intCast(std.time.nanoTimestamp() - start_ns);
                const tag_name = @tagName(instruction);
                // Intern tag for stable key and minimal allocations
                const key = self.string_interner.intern(tag_name) catch tag_name;
                var gop = self.profile_map.getOrPut(key) catch unreachable;
                if (!gop.found_existing) gop.value_ptr.* = .{ .count = 0, .ns = 0 };
                gop.value_ptr.count += 1;
                gop.value_ptr.ns += elapsed;
            }

            // Only advance IP if instruction didn't jump
            if (!self.didJump(instruction)) {
                self.ip += 1;
            }
        }

        // Return final result from stack if available
        if (self.stack.size() > 0) {
            const result = try self.stack.pop();
            if (self.profile_mode) self.dumpProfile() catch {};
            return result;
        }

        if (self.profile_mode) self.dumpProfile() catch {};
        return null;
    }

    /// CRITICAL: Validate that no Unknown types exist in the HIR program
    fn validateHIRProgram(self: *HIRVM) !void {
        for (self.program.instructions, 0..) |instruction, ip| {
            switch (instruction) {
                .Peek => |i| {
                    if (i.value_type == .Unknown) {
                        self.reporter.reportCompileError(
                            Location{
                                .file = "unknown",
                                .range = .{
                                    .start_line = 0,
                                    .start_col = 0,
                                    .end_line = 0,
                                    .end_col = 0,
                                },
                            },
                            ErrorCode.UNKNOWN_TYPE_FOUND_IN_PEEK_INSTRUCTION,
                            "FATAL: Unknown type found in Peek instruction at IP {} for variable '{s}'. Type inference likely failed during code generation.",
                            .{ ip, i.name orelse "unknown" },
                        );
                        return ErrorList.InternalParserError;
                    }
                },

                .PeekStruct => |i| {
                    // Check for Unknown types in field types
                    for (i.field_types) |field_type| {
                        if (field_type == .Unknown) {
                            self.reporter.reportCompileError(
                                Location{
                                    .file = "unknown",
                                    .range = .{
                                        .start_line = 0,
                                        .start_col = 0,
                                        .end_line = 0,
                                        .end_col = 0,
                                    },
                                },
                                ErrorCode.UNKNOWN_TYPE_FOUND_IN_PEEK_INSTRUCTION,
                                "FATAL: Unknown type found in PeekStruct instruction at IP {} for field {}. Type inference likely failed during code generation.",
                                .{ ip, i },
                            );
                            return ErrorList.InternalParserError;
                        }
                    }
                },

                .Compare => |c| {
                    if (c.operand_type == .Unknown) {
                        self.reporter.reportCompileError(
                            Location{
                                .file = "unknown",
                                .range = .{
                                    .start_line = 0,
                                    .start_col = 0,
                                    .end_line = 0,
                                    .end_col = 0,
                                },
                            },
                            ErrorCode.UNKNOWN_TYPE_FOUND_IN_PEEK_INSTRUCTION,
                            "FATAL: Auto operand_type found in Compare instruction at IP {}. Type inference likely failed during code generation.",
                            .{ip},
                        );
                        return ErrorList.InternalParserError;
                    }
                },
                .Convert => |c| {
                    if (c.from_type == .Unknown or c.to_type == .Unknown) {
                        self.reporter.reportCompileError(
                            Location{
                                .file = "unknown",
                                .range = .{
                                    .start_line = 0,
                                    .start_col = 0,
                                    .end_line = 0,
                                    .end_col = 0,
                                },
                            },
                            ErrorCode.UNKNOWN_TYPE_FOUND_IN_PEEK_INSTRUCTION,
                            "FATAL: Unknown type found in Convert instruction at IP {} (from: {s}, to: {s}). Type inference likely failed during code generation.",
                            .{ ip, @tagName(c.from_type), @tagName(c.to_type) },
                        );
                        return ErrorList.InternalParserError;
                    }
                },
                // unknown types are allowed here as we allow union and dynamic cases to flow without concrete return types
                // TODO see if we can make this more strict to not allow unknown types past this point
                .Call => |c| {
                    _ = c;
                },
                .TailCall => |c| {
                    _ = c;
                },
                .Return => |r| {
                    _ = r;
                },
                else => {
                    // Other instructions don't have explicit type fields to validate
                },
            }
        }
    }

    /// Execute a single HIR instruction
    pub fn executeInstruction(self: *HIRVM, instruction: HIRInstruction) ErrorList!void {
        switch (instruction) {
            .Const => |c| {
                // Push constant from constant pool
                const constant_value = self.program.constant_pool[c.constant_id];
                try self.stack.push(HIRFrame.initFromHIRValue(constant_value));
            },

            .LoadVar => |v| {
                try decl_variables.VariableOps.execLoadVar(self, v);
            },

            .StoreVar => |v| {
                try decl_variables.VariableOps.execStoreVar(self, v);
            },

            .StoreConst => |v| {
                try decl_variables.VariableOps.execStoreConst(self, v);
            },

            .PushStorageId => |p| {
                try decl_variables.VariableOps.execPushStorageId(self, p);
            },

            .StoreParamAlias => |s| {
                try decl_variables.VariableOps.execStoreParamAlias(self, s);
            },

            .Arith => |a| {
                try ops_arith.exec(self, a);
            },

            .Compare => |c| {
                try ops_compare.exec(self, c);
            },

            .TypeCheck => |tc| {
                try ops_type.TypeOps.execTypeCheck(self, tc);
            },

            .Jump => |j| {
                try ops_control.ControlFlowOps.execJump(self, j);
            },

            .JumpCond => |j| {
                try ops_control.ControlFlowOps.execJumpCond(self, j);
            },

            .Label => {
                ops_control.ControlFlowOps.execLabel(self);
            },

            .Dup => {
                try ops_stack.StackOps.execDup(self);
            },

            .Pop => {
                try ops_stack.StackOps.execPop(self);
            },

            .Swap => {
                try ops_stack.StackOps.execSwap(self);
            },

            .Peek => |peek| {
                try debug_print.PrintOps.execPeek(self, peek);
            },

            .PeekStruct => |i| {
                try debug_print.PrintOps.execPeekStruct(self, i);
            },

            .Print => {
                try debug_print.PrintOps.execPrint(self);
            },

            .PrintInterpolated => |interp| {
                try debug_print.PrintOps.execPrintInterpolated(self, interp);
            },

            .EnterScope => |e| {
                // TAIL CALL FIX: Skip scope creation if this is a tail call
                if (self.skip_next_enter_scope) {
                    self.skip_next_enter_scope = false; // Reset flag

                    return; // Skip scope creation entirely
                }

                // Create new scope - the memory module handles all the complexity
                const new_scope = try self.memory_manager.scope_manager.createScope(self.current_scope, self.memory_manager);
                // Pre-size maps for upcoming variable bindings (params/locals)
                new_scope.reserveVariableCapacity(@intCast(e.var_count));
                self.current_scope = new_scope;
                // Reset hot variable cache for new scope to avoid stale entries
                self.hot_var_count = 0;
                // Optionally clear entries
                var h_i: usize = 0;
                while (h_i < self.hot_vars.len) : (h_i += 1) self.hot_vars[h_i] = null;
                // Clear per-scope variable cache to avoid stale storage_id mappings
                self.var_cache.clearRetainingCapacity();
                // Reserved: potential fast-path hint

                // Push a new defer list for this scope
                try self.defer_stacks.append(std.array_list.Managed(DeferAction).init(self.allocator));
            },

            .ExitScope => {
                // Run implicit defers registered for this scope (in reverse order)
                if (self.defer_stacks.items.len > 0) {
                    var list = &self.defer_stacks.items[self.defer_stacks.items.len - 1];
                    var d: usize = list.items.len;
                    while (d > 0) : (d -= 1) {
                        const action = list.items[d - 1];
                        action();
                    }
                    list.deinit();
                    _ = self.defer_stacks.pop();
                }

                if (self.current_scope.parent) |parent_scope| {
                    const old_scope = self.current_scope;
                    self.current_scope = parent_scope;

                    // Clean up the old scope
                    old_scope.deinit();
                }
                // Reset hot variable cache when leaving a scope
                self.hot_var_count = 0;
                var h_j: usize = 0;
                while (h_j < self.hot_vars.len) : (h_j += 1) self.hot_vars[h_j] = null;
                // Clear per-scope variable cache on scope exit as well
                self.var_cache.clearRetainingCapacity();
                // Reserved
            },

            .AssertFail => |a| {

                // Format and print assertion failure message
                const location_str = try std.fmt.allocPrint(self.allocator, "{s}:{}:{}", .{ a.location.file, a.location.range.start_line, a.location.range.start_col });
                defer self.allocator.free(location_str);

                if (a.has_message) {

                    // Get the message from the stack
                    const message = try self.stack.pop();
                    const message_str = switch (message.value) {
                        .string => |s| s,
                        else => "Invalid message type",
                    };

                    // Print formatted assertion failure
                    var stderr_buffer: [1024]u8 = undefined;
                    var stderr_writer = std.fs.File.stderr().writer(&stderr_buffer);
                    const stderr = &stderr_writer.interface;
                    try stderr.print("Assertion failed at {s}:\n{s}\n", .{ location_str, message_str });
                } else {
                    // No message provided
                    var stderr_buffer: [1024]u8 = undefined;
                    var stderr_writer = std.fs.File.stderr().writer(&stderr_buffer);
                    const stderr = &stderr_writer.interface;
                    try stderr.print("Assertion failed at {s}\n", .{location_str});
                }

                // Halt the program after assertion failure
                self.running = false;
            },

            .Halt => {
                self.running = false;
            },

            // ULTRA-FAST Logical operations using lookup tables!
            .LogicalOp => |op| {
                try ops_logical.exec(self, op);
            },

            .StringOp => |s| {
                try ops_strings.exec(self, s);
            },

            // Array operations (Phase 1: Core Data Types)
            .ArrayNew => |a| {
                try ops_array.exec(self, .{ .ArrayNew = a });
            },

            .ArrayGet => |a| {
                try ops_array.exec(self, .{ .ArrayGet = a });
            },

            .ArraySet => |a| {
                try ops_array.exec(self, .{ .ArraySet = a });
            },

            .ArrayPush => |_| {
                try ops_array.exec(self, .{ .ArrayPush = {} });
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

            .ArrayConcat => {
                try ops_array.exec(self, .{ .ArrayConcat = {} });
            },

            .Range => |r| {
                try ops_array.exec(self, .{ .Range = r });
            },

            .TailCall => |c| {
                try ops_functions.FunctionOps.execTailCall(self, c);
            },

            .Call => |c| {
                // Enter function profiling region for local functions
                if (self.profile_mode and c.call_kind == .LocalFunction) self.in_function_depth += 1;
                try ops_functions.FunctionOps.execCall(self, c);
            },

            .Return => |r| {
                try ops_functions.FunctionOps.execReturn(self, r);
                if (self.profile_mode and self.in_function_depth > 0) self.in_function_depth -= 1;
            },

            .TryBegin => |t| {
                // Create a new scope for the try block
                const new_scope = try self.memory_manager.scope_manager.createScope(self.current_scope, self.memory_manager);
                self.current_scope = new_scope;

                // Store the catch label for later use
                if (self.label_map.get(t.catch_label)) |target_ip| {
                    // Store the catch target for use by Throw
                    self.current_catch_target = target_ip;
                } else {
                    return self.reporter.reportRuntimeError(null, ErrorCode.VARIABLE_NOT_FOUND, "Catch label not found: {s}", .{t.catch_label});
                }
            },

            .TryCatch => |t| {
                // Exit the try block scope and create a new scope for the catch block
                if (self.current_scope.parent) |parent_scope| {
                    const old_scope = self.current_scope;
                    self.current_scope = parent_scope;
                    old_scope.deinit();
                }

                // Create new scope for catch block
                const catch_scope = try self.memory_manager.scope_manager.createScope(self.current_scope, self.memory_manager);
                self.current_scope = catch_scope;

                // If there's an exception type, we could store it for type checking
                _ = t.exception_type;
            },

            .Throw => |t| {
                // For now, we'll just push nothing onto the stack as the error value
                try self.stack.push(HIRFrame.initNothing());

                // Jump to the catch block if we have one
                if (self.current_catch_target) |target_ip| {
                    self.ip = target_ip;
                } else {
                    return self.reporter.reportRuntimeError(null, ErrorCode.VARIABLE_NOT_FOUND, "No catch block found for throw with type {s}", .{@tagName(t.exception_type)});
                }
            },

            .StructNew => |s| {
                // Allocate array for fields
                var fields = try self.allocator.alloc(HIRStructField, s.field_count);

                // Pop field names and values from stack and create fields in reverse order
                // Stack order: [value1, name1, value2, name2, ...]
                var i: usize = s.field_count;
                while (i > 0) {
                    i -= 1;
                    const field_name = try self.stack.pop(); // Pop field name
                    const field_value = try self.stack.pop(); // Pop field value

                    // Extract field name as string
                    const name_str = switch (field_name.value) {
                        .string => |s_name| s_name,
                        else => {
                            self.reporter.reportRuntimeError(null, ErrorCode.VARIABLE_NOT_FOUND, "Expected string for field name, got {s}", .{@tagName(field_name.value)});
                            return ErrorList.TypeError;
                        },
                    };

                    fields[i] = HIRStructField{
                        .name = try self.string_interner.intern(name_str),
                        .value = field_value.value,
                        .field_type = s.field_types[i],
                        .path = null,
                    };
                }

                // Create new struct value
                const struct_value = HIRValue{ .struct_instance = .{
                    .type_name = try self.allocator.dupe(u8, s.type_name),
                    .fields = fields,
                    .field_name = null,
                    .path = null,
                } };

                // Push struct onto stack
                try self.stack.push(HIRFrame.initFromHIRValue(struct_value));
            },

            .StoreFieldName => |store_field| {
                // Get the struct instance from the top of the stack
                const frame = try self.stack.pop();

                // Ensure we have a struct instance
                switch (frame.value) {
                    .struct_instance => |struct_inst| {
                        // Check if field names are already set (modern StructNew behavior)
                        var has_empty_fields = false;
                        for (struct_inst.fields) |field| {
                            if (field.name.len == 0) {
                                has_empty_fields = true;
                                break;
                            }
                        }

                        if (has_empty_fields) {
                            // Legacy behavior: find the first empty field name and set it
                            for (struct_inst.fields) |*field| {
                                if (field.name.len == 0) {
                                    // Allocate and store the field name
                                    field.name = try self.string_interner.intern(store_field.field_name);
                                    break;
                                }
                            }
                        }
                        // If no empty fields, this is a no-op (field names already set by StructNew)

                        // Create a new frame with the (possibly modified) struct and push it back
                        const modified_frame = HIRFrame.initFromHIRValue(HIRValue{ .struct_instance = struct_inst });
                        try self.stack.push(modified_frame);
                    },
                    else => {
                        self.reporter.reportRuntimeError(null, ErrorCode.VARIABLE_NOT_FOUND, "Cannot store field name on non-struct value: {s}", .{@tagName(frame.value)});
                        return ErrorList.TypeError;
                    },
                }
            },

            .GetField => |get_field| {
                // Get the struct instance from the top of the stack
                const frame = try self.stack.pop();

                // Handle struct fields
                switch (frame.value) {
                    .string => {
                        // Handle module namespace field access (e.g., "graphics.raylib".WindowShouldClose or "graphics.raylib".SKYBLUE)
                        if (std.mem.startsWith(u8, frame.value.string, "graphics.")) {
                            const full_name = try std.fmt.allocPrint(self.allocator, "{s}.{s}", .{ frame.value.string, get_field.field_name });
                            try self.stack.push(HIRFrame.initFromHIRValue(HIRValue{ .string = full_name }));
                            return;
                        }

                        // Handle special string operations
                        if (std.mem.eql(u8, get_field.field_name, "length")) {
                            // String length operation
                            const result = try ops_strings.stringLength(self, frame);
                            try self.stack.push(result);
                            return;
                        } else if (std.mem.eql(u8, get_field.field_name, "bytes")) {
                            // String bytes operation
                            const result = try ops_strings.stringBytes(self, frame);
                            try self.stack.push(result);
                            return;
                        } else {
                            // Try to parse field name as index for string indexing
                            if (std.fmt.parseInt(i64, get_field.field_name, 10)) |index| {
                                // Create frames for substring operation
                                const start_frame = HIRFrame.initInt(index);
                                const len_frame = HIRFrame.initInt(1);
                                const result = try ops_strings.stringSubstring(self, frame, start_frame, len_frame);
                                try self.stack.push(result);
                                return;
                            } else |_| {
                                self.reporter.reportRuntimeError(null, ErrorCode.VARIABLE_NOT_FOUND, "Invalid string operation: {s}", .{get_field.field_name});
                                return ErrorList.TypeError;
                            }
                        }
                    },
                    .struct_instance => |struct_inst| {
                        // Special handling for graphics.App.running field - check WindowShouldClose dynamically
                        if (std.mem.eql(u8, struct_inst.type_name, "graphics.App") and std.mem.eql(u8, get_field.field_name, "running")) {
                            const ray = @import("../runtime/raylib.zig");
                            const should_close = ray.WindowShouldClose();
                            try self.stack.push(HIRFrame.initTetra(if (should_close) 0 else 1));
                            return;
                        }

                        // Intern the field name for comparison
                        const interned_name = try self.string_interner.intern(get_field.field_name);

                        // Find field by name
                        for (struct_inst.fields) |field| {
                            if ((field.name.ptr == interned_name.ptr and field.name.len == interned_name.len) or std.mem.eql(u8, field.name, interned_name)) {
                                // For nested structs, we need to set the path
                                var field_value = field.value;
                                if (field_value == .struct_instance) {
                                    // Build the full field path
                                    var field_path: []const u8 = undefined;
                                    if (struct_inst.path) |path| {
                                        field_path = try std.fmt.allocPrint(self.allocator, "{s}.{s}", .{ path, field.name });
                                    } else {
                                        field_path = try std.fmt.allocPrint(self.allocator, "{s}.{s}", .{ struct_inst.type_name, field.name });
                                    }
                                    field_value.struct_instance.path = field_path;
                                }

                                // Push the field value onto the stack
                                try self.stack.push(HIRFrame{ .value = field_value });
                                return;
                            }
                        }

                        self.reporter.reportRuntimeError(null, ErrorCode.VARIABLE_NOT_FOUND, "Field '{s}' not found in struct '{s}'", .{ get_field.field_name, struct_inst.type_name });
                        return ErrorList.FieldNotFound;
                    },
                    else => {
                        self.reporter.reportRuntimeError(null, ErrorCode.VARIABLE_NOT_FOUND, "Cannot get field from non-struct value: {s}", .{@tagName(frame.value)});
                        return ErrorList.TypeError;
                    },
                }
            },

            .SetField => |set_field| {
                // Get the value to set from the top of the stack
                const value_frame = try self.stack.pop();

                // Get the struct instance from the stack
                const struct_frame = try self.stack.pop();

                // Ensure we have a struct instance
                switch (struct_frame.value) {
                    .struct_instance => |struct_inst| {
                        // Find field by name
                        for (struct_inst.fields) |*field| {
                            if (std.mem.eql(u8, field.name, set_field.field_name)) {
                                // Update the field value
                                field.value = value_frame.value;

                                // Create a new frame with the modified struct and push it back
                                const modified_frame = HIRFrame.initFromHIRValue(HIRValue{ .struct_instance = struct_inst });
                                try self.stack.push(modified_frame);
                                return;
                            }
                        }

                        self.reporter.reportRuntimeError(null, ErrorCode.VARIABLE_NOT_FOUND, "Field '{s}' not found in struct '{s}'", .{ set_field.field_name, struct_inst.type_name });
                        return ErrorList.FieldNotFound;
                    },
                    else => {
                        self.reporter.reportRuntimeError(null, ErrorCode.VARIABLE_NOT_FOUND, "Cannot set field on non-struct value: {s}", .{@tagName(struct_frame.value)});
                        return ErrorList.TypeError;
                    },
                }
            },

            .Map => |map| {
                var entries = try self.allocator.alloc(HIRMapEntry, map.entries.len);

                // The codegen pushes entries in reverse order: [key_last, value_last, ..., key_first, value_first]
                // When we pop, we get them in reverse: value_first, key_first, value_second, key_second, etc.
                // So we need to fill entries in reverse order to get the original order
                var reverse_i = entries.len;
                while (reverse_i > 0) {
                    reverse_i -= 1;
                    const value = try self.stack.pop(); // Pop value first
                    const key = try self.stack.pop(); // Pop key second

                    const normalized_key: HIRValue = switch (key.value) {
                        .string => |s| blk_key: {
                            const interned = try self.string_interner.intern(s);
                            break :blk_key HIRValue{ .string = interned };
                        },
                        else => key.value,
                    };

                    entries[reverse_i] = HIRMapEntry{
                        .key = normalized_key,
                        .value = value.value,
                    };
                }

                const map_value = HIRValue{ .map = HIRMap{
                    .entries = entries,
                    .key_type = map.key_type,
                    .value_type = map.value_type,
                } };

                try self.stack.push(HIRFrame.initFromHIRValue(map_value));
            },

            .MapGet => {
                // Get map value by key
                var key = try self.stack.pop();
                switch (key.value) {
                    .string => |s| {
                        const interned = try self.string_interner.intern(s);
                        key.value = HIRValue{ .string = interned };
                    },
                    else => {},
                }
                const map = try self.stack.pop();

                if (self.reporter.debug_mode) {
                    const kt = debug_print.getTypeString(self, key.value);
                    const mt = debug_print.getTypeString(self, map.value);
                    self.reporter.report(.Debug, .Hint, null, null, "MapGet key_type='{s}' map_type='{s}'", .{ kt, mt });
                }

                switch (map.value) {
                    .map => |m| {

                        // Find entry with matching key
                        var found = false;
                        for (m.entries) |entry| {

                            // Use custom comparison for HIRValue keys
                            const keys_match = switch (entry.key) {
                                .string => |entry_str| switch (key.value) {
                                    .string => |key_str| ((entry_str.ptr == key_str.ptr and entry_str.len == key_str.len) or std.mem.eql(u8, entry_str, key_str)),
                                    else => false,
                                },
                                .int => |entry_int| switch (key.value) {
                                    .int => |key_int| entry_int == key_int,
                                    else => false,
                                },
                                else => false,
                            };

                            if (keys_match) {
                                try self.stack.push(HIRFrame.initFromHIRValue(entry.value));
                                found = true;
                                break;
                            }
                        }

                        if (!found) {
                            // Key not found - return nothing
                            try self.stack.push(HIRFrame.initFromHIRValue(HIRValue.nothing));
                        }
                    },
                    else => return self.reporter.reportRuntimeError(null, ErrorCode.VARIABLE_NOT_FOUND, "Cannot get from non-map value: {s}", .{@tagName(map.value)}),
                }
            },

            .MapSet => |m| {
                // Set map value by key
                // Stack order (top to bottom): value, key, map
                const value = try self.stack.pop();
                var key = try self.stack.pop();
                switch (key.value) {
                    .string => |s| {
                        const interned = try self.string_interner.intern(s);
                        key.value = HIRValue{ .string = interned };
                    },
                    else => {},
                }
                const map_frame = try self.stack.pop();

                if (self.reporter.debug_mode) {
                    const vt = debug_print.getTypeString(self, value.value);
                    const kt = debug_print.getTypeString(self, key.value);
                    const mt = debug_print.getTypeString(self, map_frame.value);
                    self.reporter.report(.Debug, .Hint, null, null, "MapSet value='{s}' key='{s}' map='{s}'", .{ vt, kt, mt });
                }

                switch (map_frame.value) {
                    .map => |orig| {
                        // Create a mutable copy of entries
                        var entries = try self.allocator.alloc(HIRMapEntry, orig.entries.len);
                        @memcpy(entries, orig.entries);

                        // Try to find existing key
                        var updated = false;
                        for (entries) |*entry| {
                            const keys_match = switch (entry.key) {
                                .string => |entry_str| switch (key.value) {
                                    .string => |key_str| ((entry_str.ptr == key_str.ptr and entry_str.len == key_str.len) or std.mem.eql(u8, entry_str, key_str)),
                                    else => false,
                                },
                                .int => |entry_int| switch (key.value) {
                                    .int => |key_int| entry_int == key_int,
                                    else => false,
                                },
                                else => false,
                            };
                            if (keys_match) {
                                entry.value = value.value;
                                updated = true;
                                break;
                            }
                        }

                        if (!updated) {
                            // Append new entry
                            var new_entries = try self.allocator.alloc(HIRMapEntry, entries.len + 1);
                            @memcpy(new_entries[0..entries.len], entries);
                            new_entries[entries.len] = HIRMapEntry{ .key = key.value, .value = value.value };
                            self.allocator.free(entries);
                            entries = new_entries;
                        }

                        const new_map = HIRValue{ .map = HIRMap{
                            .entries = entries,
                            .key_type = m.key_type,
                            .value_type = .Unknown,
                        } };
                        try self.stack.push(HIRFrame.initFromHIRValue(new_map));
                    },
                    else => return self.reporter.reportRuntimeError(null, ErrorCode.VARIABLE_NOT_FOUND, "Cannot set index on non-map value: {s}", .{@tagName(map_frame.value)}),
                }
            },

            .Convert => |c| {
                try ops_type.TypeOps.execConvert(self, c);
            },
            .ArrayLen => {
                try ops_array.exec(self, .{ .ArrayLen = {} });
            },
            else => {
                // Print detailed instruction info based on type
                std.debug.print("  Raw data: {any}\n", .{instruction});
                return ErrorList.NotImplemented;
            },
        }
    }

    /// Check if the last instruction caused a jump (to avoid auto-incrementing IP)
    fn didJump(self: *HIRVM, instruction: HIRInstruction) bool {
        _ = self;
        return switch (instruction) {
            .Jump, .JumpCond => true,
            .Call => |c| c.call_kind == .LocalFunction, // Function calls jump to function start
            .TailCall => true, // Tail calls always jump to function start
            .Return => true, // ALL Return instructions set IP manually, never auto-increment
            else => false,
        };
    }

    /// Convert HIR value to string representation
    pub fn valueToString(self: *HIRVM, value: HIRValue) ![]const u8 {
        return switch (value) {
            .int => |i| try std.fmt.allocPrint(self.allocator, "{}", .{i}),
            .byte => |u| try std.fmt.allocPrint(self.allocator, "0x{X:0>2}", .{u}),
            .float => |f| {
                const roundedDown = std.math.floor(f);
                if (f - roundedDown == 0) {
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
                    if (std.meta.eql(elem, HIRValue.nothing)) break; // Stop at first nothing element
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
                for (s.fields, 0..) |field, i| {
                    if (i > 0) try result.appendSlice(", ");
                    try result.appendSlice(field.name);
                    try result.appendSlice(": ");
                    const field_str = try self.valueToString(field.value);
                    defer self.allocator.free(field_str);
                    // Quote embedded string fields for clearer struct representation
                    switch (field.value) {
                        .string => {
                            try result.append('"');
                            try result.appendSlice(field_str);
                            try result.append('"');
                        },
                        else => try result.appendSlice(field_str),
                    }
                }
                try result.appendSlice(" }");
                return try result.toOwnedSlice();
            },
            .enum_variant => |e| try std.fmt.allocPrint(self.allocator, ".{s}", .{e.variant_name}),
            .storage_id_ref => |storage_id| try std.fmt.allocPrint(self.allocator, "storage_id_ref({})", .{storage_id}),
            else => try std.fmt.allocPrint(self.allocator, "{s}", .{@tagName(value)}),
        };
    }

    /// Debug helper to dump current VM state
    pub fn dumpState(self: *HIRVM) void {
        std.debug.print("\n=== HIR VM State Dump ===\n", .{});
        std.debug.print("IP: {}, Running: {}, Stack size: {}\n", .{ self.ip, self.running, self.stack.size() });

        // Dump memory state
        self.memory_manager.scope_manager.dumpState(self.current_scope.id);

        // Dump stack contents
        if (self.stack.size() > 0) {
            std.debug.print("Stack contents (top to bottom):\n", .{});
            var i: i64 = self.stack.sp - 1;
            while (i >= 0) : (i -= 1) {
                const frame = self.stack.data[@intCast(i)];
                std.debug.print("  [{}]: ", .{i});
                try debug_print.printHIRValue(self, frame.value);
                std.debug.print("\n", .{});
            }
        } else {
            std.debug.print("Stack is empty\n", .{});
        }
        std.debug.print("=========================\n\n", .{});
    }

    /// Dump profiling summary (sorted by total time desc)
    fn dumpProfile(self: *HIRVM) !void {
        const ProfileRow = struct { name: []const u8, count: u64, ns: u64 };
        var entries = try self.allocator.alloc(ProfileRow, self.profile_map.count());
        defer self.allocator.free(entries);
        var idx: usize = 0;
        var it = self.profile_map.iterator();
        while (it.next()) |e| {
            entries[idx] = .{ .name = e.key_ptr.*, .count = e.value_ptr.count, .ns = e.value_ptr.ns };
            idx += 1;
        }
        const Cmp = struct {
            fn less(_: void, a: ProfileRow, b: ProfileRow) bool {
                return a.ns > b.ns; // Desc
            }
        };
        std.sort.block(ProfileRow, entries, {}, Cmp.less);

        var stdout_buffer: [1024]u8 = undefined;
        var stdout_writer = std.fs.File.stdout().writer(&stdout_buffer);
        const out = &stdout_writer.interface;
        try out.print("\n=== PROFILE (inside functions) ===\n", .{});
        var total_ns: u128 = 0;
        for (entries) |en| total_ns += en.ns;
        for (entries) |en| {
            const pct: f64 = if (total_ns == 0) 0 else @as(f64, @floatFromInt(en.ns)) * 100.0 / @as(f64, @floatFromInt(total_ns));
            try out.print("{s:18}  count={d:8}  time={d:12} ns  ({d:.2}%)\n", .{ en.name, en.count, en.ns, pct });
        }
        try out.print("Total: {d} ns\n", .{total_ns});
    }

    pub fn exitScope(self: *HIRVM, scope_id: u32) !void {
        // Get the scope
        var current_scope: ?*Scope = self.current_scope;
        while (current_scope) |scope| {
            if (scope.id == scope_id) {
                const frame = try self.stack.peek();
                if (frame.scope_refs == 0) {
                    scope.deinit();
                }
                return;
            }
            current_scope = scope.parent;
        }
    }

    pub fn executeCall(self: *HIRVM, function: HIRFunction, args: []HIRValue) !void {
        // Create new scope for function
        const new_scope = try self.memory_manager.scope_manager.createScope(self.current_scope, self.memory_manager);

        // Get current frame and increment its scope reference
        var frame = try self.stack.peek();
        frame.incrementScopeRefs();

        // Add function arguments to new scope
        // We use arg_0, arg_1, etc. as parameter names since we don't have the actual names
        for (args, 0..) |arg, i| {
            const param_name = try std.fmt.allocPrint(self.allocator, "arg_{d}", .{i});
            defer self.allocator.free(param_name);

            _ = try new_scope.createValueBinding(param_name, self.hirValueToTokenLiteral(arg), self.hirValueToTokenType(arg), self.hirValueToTypeInfo(arg), false);
        }

        // Update current scope
        self.current_scope = new_scope;

        // Find the function's start label in the program's instruction array
        const start_label_idx = self.label_map.get(function.start_label) orelse {
            return self.reporter.reportRuntimeError(null, ErrorCode.VARIABLE_NOT_FOUND, "Function start label not found: {s}", .{function.start_label});
        };

        // Save current execution state
        const saved_ip = self.ip;
        const saved_running = self.running;

        // Set up execution environment for function
        self.ip = start_label_idx;
        self.running = true;

        // Execute instructions until we hit a Return
        while (self.running and self.ip < self.program.instructions.len) {
            const instruction = self.program.instructions[self.ip];
            try self.executeInstruction(instruction);

            // Only advance IP if instruction didn't jump
            if (!self.didJump(instruction)) {
                self.ip += 1;
            }
        }

        // Restore original execution state
        self.ip = saved_ip;
        self.running = saved_running;

        // Decrement scope reference before exiting
        frame = try self.stack.peek();
        frame.decrementScopeRefs();

        // Exit function scope
        try self.exitScope(new_scope.id);
    }

    fn handleQuantifier(self: *HIRVM, array_frame: HIRFrame, predicate: HIRValue, is_exists: bool) !void {
        switch (array_frame.value) {
            .array => |arr| {
                var result = if (is_exists) false else true;

                // Apply predicate to each element
                for (arr.elements) |element| {
                    // Skip nothing values
                    if (element == .nothing) continue;

                    // Execute predicate with current element
                    try self.stack.push(HIRFrame.initFromHIRValue(element));

                    // Get the predicate function from the function table
                    const function_index = @as(usize, @intCast(predicate.int));
                    const function = self.program.function_table[function_index];

                    // Create a mutable slice for the arguments
                    var args = try self.allocator.alloc(HIRValue, 1);
                    args[0] = element;
                    try self.executeCall(function, args);
                    self.allocator.free(args);

                    const pred_result = try self.stack.pop();

                    if (pred_result.value == .tetra) {
                        const bool_result = pred_result.value.tetra == 1;
                        if (is_exists and bool_result) {
                            result = true;
                            break;
                        } else if (!is_exists and !bool_result) {
                            result = false;
                            break;
                        }
                    }
                }

                try self.stack.push(HIRFrame.initTetra(if (result) 1 else 0));
            },
            .nothing => {
                // Return false for exists, true for forall on empty/nothing
                try self.stack.push(HIRFrame.initTetra(if (is_exists) 0 else 1));
            },
            else => {
                const op_name = if (is_exists) "exists_quantifier" else "forall_quantifier";
                return self.reporter.reportRuntimeError(null, ErrorCode.VARIABLE_NOT_FOUND, "{s}: argument must be array", .{op_name});
            },
        }
    }

    /// Execute a list of instructions
    pub fn executeInstructions(self: *HIRVM, instruction_list: []const HIRInstruction) !void {
        const saved_ip = self.ip;
        const saved_running = self.running;

        // Set up temporary execution environment
        self.ip = 0;
        self.running = true;

        // Execute instructions
        while (self.running and self.ip < instruction_list.len) {
            const instruction = instruction_list[self.ip];
            try self.executeInstruction(instruction);

            // Only advance IP if instruction didn't jump
            if (!self.didJump(instruction)) {
                self.ip += 1;
            }
        }

        // Restore original execution state
        self.ip = saved_ip;
        self.running = saved_running;
    }

    /// Coerce a value to the expected type if possible
    pub fn coerceValue(self: *HIRVM, value: HIRValue, expected_type: HIRType) HIRValue {
        _ = self;
        // If expected type is Unknown, no coercion needed
        if (expected_type == .Unknown) {
            return value;
        }

        // Perform type coercion based on expected type
        const result = switch (expected_type) {
            .Float => switch (value) {
                .int => |i| HIRValue{ .float = @floatFromInt(i) },
                .byte => |u| HIRValue{ .float = @floatFromInt(u) },
                .float => value, // Already correct type
                else => value, // Cannot coerce, return original
            },
            .Byte => switch (value) {
                .int => |i| if (i >= 0 and i <= 255) HIRValue{ .byte = @intCast(i) } else value,
                .byte => value, // Already correct type
                else => value, // Cannot coerce, return original
            },
            .Int => switch (value) {
                .byte => |u| HIRValue{ .int = @intCast(u) },
                .int => value, // Already correct type
                else => value, // Cannot coerce, return original
            },
            else => value, // No coercion for other types
        };

        return result;
    }

    pub fn getDefaultValue(hir_type: HIRType) HIRValue {
        return switch (hir_type) {
            .Int => HIRValue{ .int = 0 },
            .Byte => HIRValue{ .byte = 0 },
            .Float => HIRValue{ .float = 0.0 },
            .String => HIRValue{ .string = "" },
            .Tetra => HIRValue{ .tetra = 0 }, // false
            .Nothing => HIRValue.nothing,
            else => HIRValue.nothing, // Default for complex types
        };
    }

    fn getDefaultValueForArrayType(self: *HIRVM, element_type: HIRType) HIRValue {
        _ = self;
        return switch (element_type) {
            .Int => HIRValue{ .int = 0 },
            .Byte => HIRValue{ .byte = 0 },
            .Float => HIRValue{ .float = 0.0 },
            .String => HIRValue{ .string = "" },
            .Tetra => HIRValue{ .tetra = 0 }, // false
            .Nothing => HIRValue.nothing,
            else => HIRValue.nothing, // Default for complex types
        };
    }
};
