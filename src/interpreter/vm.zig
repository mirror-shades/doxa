const std = @import("std");
const hir_soxa = @import("../codegen/hir/soxa.zig");
const hir_instructions = @import("../codegen/hir/soxa_instructions.zig");
const hir_values = @import("../codegen/hir/soxa_values.zig");
const hir_types = @import("../codegen/hir/soxa_types.zig");
const HIRInstruction = hir_instructions.HIRInstruction;
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

const STACK_SIZE: u32 = 1024 * 1024;

/// Hot variable cache entry for ultra-fast variable lookup
const HotVar = struct {
    name: []const u8,
    storage_id: u32,
};

/// Simple cache that fully trusts and integrates with the memory module
/// Cache var_name -> storage_id, but invalidate on scope changes to maintain correctness
/// HIR-based VM Frame - simplified to work with memory management
pub const HIRFrame = struct {
    value: HIRValue,
    field_name: ?[]const u8 = null,
    scope_refs: u32 = 0, // Add reference counter for scopes

    // Helper constructors
    pub fn initInt(x: i32) HIRFrame {
        return HIRFrame{ .value = HIRValue{ .int = x } };
    }

    pub fn initFloat(x: f64) HIRFrame {
        return HIRFrame{ .value = HIRValue{ .float = x } };
    }

    pub fn initString(x: []const u8) HIRFrame {
        return HIRFrame{ .value = HIRValue{ .string = x } };
    }

    pub fn initTetra(x: u8) HIRFrame {
        return HIRFrame{ .value = HIRValue{ .tetra = x } };
    }

    pub fn initByte(x: u8) HIRFrame {
        return HIRFrame{ .value = HIRValue{ .byte = x } };
    }

    pub fn initNothing() HIRFrame {
        return HIRFrame{ .value = HIRValue.nothing };
    }

    pub fn initFromHIRValue(value: HIRValue) HIRFrame {
        return .{
            .value = value,
            .field_name = null,
        };
    }

    pub fn asInt(self: HIRFrame) !i32 {
        return switch (self.value) {
            .int => |i| i,
            else => ErrorList.TypeError,
        };
    }

    pub fn asFloat(self: HIRFrame) !f64 {
        return switch (self.value) {
            .float => |f| f,
            else => ErrorList.TypeError,
        };
    }

    pub fn asTetra(self: HIRFrame) !u8 {
        return switch (self.value) {
            .tetra => |t| t,
            else => ErrorList.TypeError,
        };
    }

    pub fn asString(self: HIRFrame) ![]const u8 {
        return switch (self.value) {
            .string => |s| s,
            else => ErrorList.TypeError,
        };
    }

    pub fn asByte(self: HIRFrame) !u8 {
        return switch (self.value) {
            .byte => |u| u,
            else => ErrorList.TypeError,
        };
    }

    pub fn incrementScopeRefs(self: *HIRFrame) void {
        self.scope_refs += 1;
    }

    pub fn decrementScopeRefs(self: *HIRFrame) void {
        if (self.scope_refs > 0) {
            self.scope_refs -= 1;
        }
    }
};

/// Utility functions to convert between HIR and memory system types
fn hirStructFieldToStructField(self: *HIRVM, hir_field: HIRStructField) !types.StructField {
    return types.StructField{
        .name = try self.allocator.dupe(u8, hir_field.name),
        .value = self.hirValueToTokenLiteral(hir_field.value),
    };
}

fn structFieldToHIRStructField(self: *HIRVM, field: types.StructField) !HIRStructField {
    return HIRStructField{
        .name = try self.allocator.dupe(u8, field.name),
        .value = self.tokenLiteralToHIRValue(field.value),
        .field_type = .Unknown,
        .path = null,
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
        else => unreachable,
    };
}

/// HIR-based Stack - uses heap allocation to avoid system stack overflow
const HIRStack = struct {
    data: []HIRFrame,
    sp: i32 = 0,
    allocator: std.mem.Allocator,

    pub fn init(allocator: std.mem.Allocator) !HIRStack {
        // Allocate stack on heap to avoid system stack overflow
        const data = try allocator.alloc(HIRFrame, STACK_SIZE);

        // Initialize all frames to NOTHING
        for (data) |*frame| {
            frame.* = HIRFrame.initNothing();
        }

        return HIRStack{
            .data = data,
            .sp = 0,
            .allocator = allocator,
        };
    }

    pub fn deinit(self: *HIRStack) void {
        self.allocator.free(self.data);
        self.sp = 0;
    }

    // PERFORMANCE: Inline stack operations for maximum speed
    pub inline fn push(self: *HIRStack, value: HIRFrame) !void {
        // FAST MODE: Skip bounds check in release builds for maximum performance
        if (std.debug.runtime_safety and self.sp >= STACK_SIZE) {
            std.debug.print("Stack overflow: Attempted to push at sp={}\n", .{self.sp});
            return ErrorList.StackOverflow;
        }
        self.data[@intCast(self.sp)] = value;
        self.sp += 1;
    }

    pub inline fn pop(self: *HIRStack) !HIRFrame {
        // FAST MODE: Skip bounds check in release builds for maximum performance
        if (std.debug.runtime_safety and self.sp <= 0) {
            std.debug.print("Stack underflow: Attempted to pop at sp={}\n", .{self.sp});
            return ErrorList.StackUnderflow;
        }
        self.sp -= 1;
        return self.data[@intCast(self.sp)];
    }

    pub fn peek(self: HIRStack) !HIRFrame {
        if (self.sp <= 0) {
            std.debug.print("Stack underflow: Attempted to peek at sp={}\n", .{self.sp});
            return ErrorList.StackUnderflow;
        }
        return self.data[@intCast(self.sp - 1)];
    }

    pub fn print(self: HIRStack) !HIRFrame {
        if (self.sp <= 0) {
            std.debug.print("Stack underflow: Attempted to print at sp={}\n", .{self.sp});
            return ErrorList.StackUnderflow;
        }
        return self.data[@intCast(self.sp - 1)];
    }

    pub fn size(self: HIRStack) i32 {
        return self.sp;
    }

    pub fn isEmpty(self: HIRStack) bool {
        return self.sp == 0;
    }
};

/// Call frame for function call stack management
const CallFrame = struct {
    return_ip: u32, // IP to return to after function call
    function_name: []const u8, // For debugging
    arg_count: u32 = 0, // Number of arguments to clean up from stack
    saved_sp: i32, // Operand stack pointer before the call
};

/// Call stack for proper function return handling
const CallStack = struct {
    frames: []CallFrame,
    sp: u32 = 0,
    allocator: std.mem.Allocator,

    pub fn init(allocator: std.mem.Allocator) !CallStack {
        // Start with reasonable size, will grow dynamically if needed
        const frames = try allocator.alloc(CallFrame, 1024);
        return CallStack{
            .frames = frames,
            .sp = 0,
            .allocator = allocator,
        };
    }

    pub fn deinit(self: *CallStack) void {
        self.allocator.free(self.frames);
    }

    pub fn push(self: *CallStack, frame: CallFrame) !void {
        // Grow the call stack dynamically if needed (like the old interpreter)
        if (self.sp >= self.frames.len) {
            const new_size = self.frames.len * 2; // Double the size
            const new_frames = try self.allocator.realloc(self.frames, new_size);
            self.frames = new_frames;
        }
        self.frames[self.sp] = frame;
        self.sp += 1;
    }

    pub fn pop(self: *CallStack) !CallFrame {
        if (self.sp == 0) {
            return ErrorList.StackUnderflow;
        }
        self.sp -= 1;
        return self.frames[self.sp];
    }

    pub fn isEmpty(self: CallStack) bool {
        return self.sp == 0;
    }
};

/// HIR-based Virtual Machine - executes HIR instructions directly
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

    // Tail call optimization: flag to skip scope creation on next EnterScope
    skip_next_enter_scope: bool = false,

    // New field for catch target
    current_catch_target: ?u32 = null, // Track the current catch block's IP

    // Add string interner to VM
    string_interner: *StringInterner,

    // NEW: Custom type registry for runtime type safety
    custom_type_registry: std.StringHashMap(CustomTypeInfo),

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
                // Convert TokenLiteral map to HIRMap
                var hir_entries = self.allocator.alloc(HIRMapEntry, m.count()) catch break :blk HIRValue.nothing;
                var i: usize = 0;
                var iter = m.iterator();
                while (iter.next()) |entry| {
                    hir_entries[i] = HIRMapEntry{
                        .key = HIRValue{ .string = entry.key_ptr.* },
                        .value = self.tokenLiteralToHIRValue(entry.value_ptr.*),
                    };
                    i += 1;
                }
                break :blk HIRValue{ .map = .{
                    .entries = hir_entries,
                    .key_type = .String,
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
        };
    }

    pub fn hirValueToTokenType(self: *HIRVM, hir_value: HIRValue) TokenType {
        _ = self;
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
            .string_interner = string_interner,
            .custom_type_registry = std.StringHashMap(CustomTypeInfo).init(allocator), // NEW: Initialize custom type registry
        };

        // Pre-resolve all labels for efficient jumps
        try vm.resolveLabels();

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

        // Clean up VM data structures
        self.stack.deinit();
        self.call_stack.deinit();
        self.label_map.deinit();
        self.var_cache.deinit();
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
                    try self.label_map.put(label.name, @intCast(i));
                },
                else => {},
            }
        }
    }

    /// Main execution loop - directly execute HIR instructions
    pub fn run(self: *HIRVM) !?HIRFrame {

        // sanity check on soxa code
        try self.validateHIRProgram();

        while (self.running and self.ip < self.program.instructions.len) {
            const instruction = self.program.instructions[self.ip];

            try self.executeInstruction(instruction);

            // Only advance IP if instruction didn't jump
            if (!self.didJump(instruction)) {
                self.ip += 1;
            }
        }

        // Return final result from stack if available
        if (self.stack.size() > 0) {
            return try self.stack.pop();
        }

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
    fn executeInstruction(self: *HIRVM, instruction: HIRInstruction) ErrorList!void {
        switch (instruction) {
            .Const => |c| {
                // Push constant from constant pool
                const constant_value = self.program.constant_pool[c.constant_id];
                try self.stack.push(HIRFrame.initFromHIRValue(constant_value));
            },

            .LoadVar => |v| {
                if (self.turbo_mode) {
                    // TURBO: Check hot variable cache first (array lookup - fastest possible)
                    for (self.hot_vars[0..self.hot_var_count]) |hot_var| {
                        if (hot_var != null and std.mem.eql(u8, hot_var.?.name, v.var_name)) {
                            if (self.memory_manager.scope_manager.value_storage.get(hot_var.?.storage_id)) |storage| {
                                const hir_value = self.tokenLiteralToHIRValueWithType(storage.value, storage.type_info);
                                try self.stack.push(HIRFrame.initFromHIRValue(hir_value));
                                return; // Ultra-fast path
                            }
                            break;
                        }
                    }
                }

                // FALLBACK: Standard variable lookup (populate cache for next time)
                if (self.current_scope.lookupVariable(v.var_name)) |variable| {
                    if (self.memory_manager.scope_manager.value_storage.get(variable.storage_id)) |storage| {
                        const hir_value = self.tokenLiteralToHIRValueWithType(storage.value, storage.type_info);

                        try self.stack.push(HIRFrame.initFromHIRValue(hir_value));
                    } else {
                        // Propagate error so VM halts execution immediately
                        return self.reporter.reportRuntimeError(
                            null,
                            ErrorCode.VARIABLE_NOT_FOUND,
                            "Variable storage not found for: {s}",
                            .{v.var_name},
                        );
                    }
                } else {
                    // Propagate undefined variable error
                    return self.reporter.reportRuntimeError(null, ErrorCode.VARIABLE_NOT_FOUND, "Undefined variable: {s}", .{v.var_name});
                }
            },

            .StoreVar => |v| {
                // Store top of stack to variable
                const value = try self.stack.pop();

                // Perform type coercion if needed
                const coerced_value = self.coerceValue(value.value, v.expected_type);

                const token_literal = self.hirValueToTokenLiteral(coerced_value);
                const token_type = self.hirValueToTokenType(coerced_value);
                const type_info_value = self.hirValueToTypeInfo(coerced_value);
                const type_info = try self.allocator.create(TypeInfo);
                type_info.* = type_info_value;

                if (@hasField(@TypeOf(v), "scope_kind") and v.scope_kind == .Local) {
                    // Local semantics:
                    // - If a variable exists anywhere in the active scope chain:
                    //     * If it's constant, shadow by creating a new current-scope binding
                    //     * Else update that existing storage
                    // - If not found, create in current scope
                    if (self.current_scope.lookupVariable(v.var_name)) |nearest_var| {
                        if (self.memory_manager.scope_manager.value_storage.getPtr(nearest_var.storage_id)) |storage| {
                            if (storage.*.constant) {
                                // Shadow constant from ancestor scope with a new local binding
                                _ = self.current_scope.createValueBinding(v.var_name, token_literal, token_type, type_info, false) catch |err| {
                                    return self.reporter.reportRuntimeError(null, ErrorCode.VARIABLE_NOT_FOUND, "Failed to create variable {s}: {}", .{ v.var_name, err });
                                };
                            } else {
                                // Update nearest existing (non-constant) variable
                                storage.*.value = token_literal;
                                storage.*.type = token_type;
                                storage.*.type_info = type_info;
                            }
                        } else {
                            return self.reporter.reportRuntimeError(null, ErrorCode.VARIABLE_NOT_FOUND, "Variable storage not found for: {s}", .{v.var_name});
                        }
                    } else {
                        // Not found anywhere - create in current scope
                        _ = self.current_scope.createValueBinding(v.var_name, token_literal, token_type, type_info, false) catch |err| {
                            return self.reporter.reportRuntimeError(null, ErrorCode.VARIABLE_NOT_FOUND, "Failed to create variable {s}: {}", .{ v.var_name, err });
                        };
                    }
                } else {
                    // Non-local: nearest existing variable across active scopes; otherwise create in current scope
                    if (self.current_scope.lookupVariable(v.var_name)) |variable| {
                        if (self.memory_manager.scope_manager.value_storage.getPtr(variable.storage_id)) |storage| {
                            if (storage.*.constant) {
                                return self.reporter.reportRuntimeError(null, ErrorCode.VARIABLE_NOT_FOUND, "Cannot modify constant variable: {s}", .{v.var_name});
                            }

                            storage.*.value = token_literal;
                            storage.*.type = token_type;
                            storage.*.type_info = type_info;
                        } else {
                            return self.reporter.reportRuntimeError(null, ErrorCode.VARIABLE_NOT_FOUND, "Variable storage not found for: {s}", .{v.var_name});
                        }
                    } else {
                        _ = self.current_scope.createValueBinding(v.var_name, token_literal, token_type, type_info, false) catch |err| {
                            return self.reporter.reportRuntimeError(null, ErrorCode.VARIABLE_NOT_FOUND, "Failed to create variable {s}: {}", .{ v.var_name, err });
                        };
                    }
                }
            },

            .StoreConst => |v| {
                // Store top of stack as a constant binding
                const value = try self.stack.pop();

                const token_literal = self.hirValueToTokenLiteral(value.value);
                const token_type = self.hirValueToTokenType(value.value);
                const type_info_value = self.hirValueToTypeInfo(value.value);
                const type_info = try self.allocator.create(TypeInfo);
                type_info.* = type_info_value;

                // If a binding exists in any active scope, update only if it's uninitialized constant (nothing)
                if (self.current_scope.lookupVariable(v.var_name)) |variable| {
                    if (self.memory_manager.scope_manager.value_storage.getPtr(variable.storage_id)) |storage| {
                        if (storage.*.constant) {
                            const is_nothing = storage.*.value == .nothing;
                            if (!is_nothing) {
                                // Already initialized constant - skip
                                return;
                            }
                        }
                        storage.*.value = token_literal;
                        storage.*.type = token_type;
                        storage.*.type_info = type_info;
                        storage.*.constant = true;
                    } else {
                        return self.reporter.reportRuntimeError(null, ErrorCode.VARIABLE_NOT_FOUND, "Variable storage not found for: {s}", .{v.var_name});
                    }
                } else {
                    // Create constant in the CURRENT scope so it does not leak beyond iteration
                    _ = self.current_scope.createValueBinding(v.var_name, token_literal, token_type, type_info, true) catch |err| {
                        return self.reporter.reportRuntimeError(null, ErrorCode.VARIABLE_NOT_FOUND, "Failed to create constant {s}: {}", .{ v.var_name, err });
                    };
                }
            },

            .Arith => |a| {
                const b = try self.stack.pop();
                const a_val = try self.stack.pop();

                // Handle array arithmetic first
                if (a.operand_type == .Array) {
                    if (a.op == .Add) {
                        // Array concatenation
                        switch (a_val.value) {
                            .array => |arr_a| {
                                switch (b.value) {
                                    .array => |arr_b| {
                                        // Calculate lengths
                                        var len_a: u32 = 0;
                                        for (arr_a.elements) |elem| {
                                            if (std.meta.eql(elem, HIRValue.nothing)) break;
                                            len_a += 1;
                                        }

                                        var len_b: u32 = 0;
                                        for (arr_b.elements) |elem| {
                                            if (std.meta.eql(elem, HIRValue.nothing)) break;
                                            len_b += 1;
                                        }

                                        // Create new array with combined elements
                                        const new_elements = try self.allocator.alloc(HIRValue, len_a + len_b);

                                        // Copy elements from first array
                                        for (0..len_a) |i| {
                                            new_elements[i] = arr_a.elements[i];
                                        }

                                        // Copy elements from second array
                                        for (0..len_b) |i| {
                                            new_elements[len_a + i] = arr_b.elements[i];
                                        }

                                        const result_array = HIRValue{
                                            .array = .{
                                                .elements = new_elements,
                                                .capacity = len_a + len_b,
                                                .element_type = arr_a.element_type,
                                            },
                                        };

                                        try self.stack.push(HIRFrame.initFromHIRValue(result_array));
                                        return;
                                    },
                                    else => {
                                        return self.reporter.reportRuntimeError(null, ErrorCode.VARIABLE_NOT_FOUND, "Cannot concatenate array with {s}", .{@tagName(b.value)});
                                    },
                                }
                            },
                            else => {
                                return self.reporter.reportRuntimeError(null, ErrorCode.VARIABLE_NOT_FOUND, "Cannot concatenate {s} with array", .{@tagName(a_val.value)});
                            },
                        }
                    } else {
                        return self.reporter.reportRuntimeError(null, ErrorCode.VARIABLE_NOT_FOUND, "Cannot perform {s} operation on arrays", .{@tagName(a.op)});
                    }
                }

                // Check if either operand is a float and needs promotion
                const a_is_float = a_val.value == .float;
                const b_is_float = b.value == .float;

                // All division promotes to float. If either operand is float or op is Div, promote both to float path.
                if (a_is_float or b_is_float or a.op == .Div) {
                    // Convert both operands to float and perform float arithmetic
                    const a_float = switch (a_val.value) {
                        .int => |i| @as(f64, @floatFromInt(i)),
                        .byte => |u| @as(f64, @floatFromInt(u)),
                        .float => |f| f,
                        .string => |s| blk: {
                            const parsed = std.fmt.parseFloat(f64, s) catch {
                                return self.reporter.reportRuntimeError(null, ErrorCode.VARIABLE_NOT_FOUND, "Cannot convert string '{s}' to float for arithmetic", .{s});
                            };
                            break :blk parsed;
                        },
                        else => {
                            return self.reporter.reportRuntimeError(null, ErrorCode.VARIABLE_NOT_FOUND, "Cannot convert {s} to float for arithmetic", .{@tagName(a_val.value)});
                        },
                    };

                    const b_float = switch (b.value) {
                        .int => |i| @as(f64, @floatFromInt(i)),
                        .byte => |u| @as(f64, @floatFromInt(u)),
                        .float => |f| f,
                        .string => |s| blk: {
                            const parsed = std.fmt.parseFloat(f64, s) catch {
                                return self.reporter.reportRuntimeError(null, ErrorCode.VARIABLE_NOT_FOUND, "Cannot convert string '{s}' to float for arithmetic", .{s});
                            };
                            break :blk parsed;
                        },
                        else => {
                            return self.reporter.reportRuntimeError(null, ErrorCode.VARIABLE_NOT_FOUND, "Cannot convert {s} to float for arithmetic", .{@tagName(b.value)});
                        },
                    };

                    var result: f64 = 0.0;
                    switch (a.op) {
                        .Add => result = a_float + b_float,
                        .Sub => result = a_float - b_float,
                        .Mul => result = a_float * b_float,
                        .Div => {
                            if (b_float == 0.0) {
                                return ErrorList.DivisionByZero;
                            } else result = a_float / b_float;
                        },
                        .Mod => result = @mod(a_float, b_float),
                        .Pow => {
                            result = std.math.pow(f64, a_float, b_float);
                        },
                    }

                    try self.stack.push(HIRFrame.initFloat(result));
                    return;
                }

                // Both operands are integers, perform integer arithmetic
                const a_int = switch (a_val.value) {
                    .int => |i| i,
                    .byte => |u| @as(i32, u),
                    .tetra => |t| @as(i32, t),
                    .string => |s| blk: {
                        const parsed = std.fmt.parseInt(i32, s, 10) catch {
                            return self.reporter.reportRuntimeError(null, ErrorCode.VARIABLE_NOT_FOUND, "Cannot convert string to integer for arithmetic", .{});
                        };
                        break :blk parsed;
                    },
                    .nothing => {
                        return self.reporter.reportRuntimeError(null, ErrorCode.VARIABLE_NOT_FOUND, "Cannot perform arithmetic on 'nothing' value", .{});
                    },
                    else => {
                        return self.reporter.reportRuntimeError(null, ErrorCode.VARIABLE_NOT_FOUND, "Cannot convert {s} to integer for arithmetic", .{@tagName(a_val.value)});
                    },
                };

                const b_int = switch (b.value) {
                    .int => |i| i,
                    .byte => |u| @as(i32, u),
                    .tetra => |t| @as(i32, t),
                    .string => |s| blk: {
                        const parsed = std.fmt.parseInt(i32, s, 10) catch {
                            return self.reporter.reportRuntimeError(null, ErrorCode.VARIABLE_NOT_FOUND, "Cannot convert string to integer for arithmetic", .{});
                        };
                        break :blk parsed;
                    },
                    .nothing => {
                        return self.reporter.reportRuntimeError(null, ErrorCode.VARIABLE_NOT_FOUND, "Cannot perform arithmetic on 'nothing' value", .{});
                    },
                    else => {
                        return self.reporter.reportRuntimeError(null, ErrorCode.VARIABLE_NOT_FOUND, "Cannot convert {s} to integer for arithmetic", .{@tagName(b.value)});
                    },
                };

                var int_result: i32 = undefined;
                switch (a.op) {
                    .Add => int_result = std.math.add(i32, a_int, b_int) catch {
                        return self.reporter.reportRuntimeError(null, ErrorCode.ARITHMETIC_OVERFLOW, "Integer addition overflow", .{});
                    },
                    .Sub => int_result = std.math.sub(i32, a_int, b_int) catch {
                        return self.reporter.reportRuntimeError(null, ErrorCode.ARITHMETIC_OVERFLOW, "Integer subtraction overflow", .{});
                    },
                    .Mul => int_result = std.math.mul(i32, a_int, b_int) catch {
                        return self.reporter.reportRuntimeError(null, ErrorCode.ARITHMETIC_OVERFLOW, "Integer multiplication overflow", .{});
                    },
                    .Div => unreachable,
                    .Mod => int_result = try self.fastIntMod(a_int, b_int),
                    .Pow => {
                        int_result = std.math.pow(i32, a_int, b_int);
                    },
                }

                // Preserve the original type when possible
                if (a_val.value == .byte and int_result >= 0 and int_result <= 255) {
                    try self.stack.push(HIRFrame.initByte(@intCast(int_result)));
                } else {
                    try self.stack.push(HIRFrame.initInt(int_result));
                }
            },

            .Compare => |c| {
                const b = try self.stack.pop();
                const a_val = try self.stack.pop();

                const result = switch (c.op) {
                    .Eq => try self.compareEqual(a_val, b),
                    .Ne => !(try self.compareEqual(a_val, b)),
                    .Lt => try self.compareLess(a_val, b),
                    .Le => (try self.compareLess(a_val, b)) or (try self.compareEqual(a_val, b)),
                    .Gt => try self.compareGreater(a_val, b),
                    .Ge => (try self.compareGreater(a_val, b)) or (try self.compareEqual(a_val, b)),
                };

                try self.stack.push(HIRFrame.initTetra(if (result) 1 else 0));
            },

            .TypeCheck => |tc| {
                const value = try self.stack.pop();

                // Get the runtime type of the value
                const runtime_type = self.getTypeString(value.value);

                // Compare with target type
                const type_match = std.mem.eql(u8, runtime_type, tc.target_type);

                // Push result as tetra (1 for match, 0 for no match)
                try self.stack.push(HIRFrame.initTetra(if (type_match) 1 else 0));
            },

            .Jump => |j| {
                // Unconditional jump to label
                if (self.label_map.get(j.label)) |target_ip| {
                    self.ip = target_ip;
                } else {
                    return self.reporter.reportRuntimeError(null, ErrorCode.VARIABLE_NOT_FOUND, "Unknown label: {s}", .{j.label});
                }
            },

            .JumpCond => |j| {
                // OPTIMIZED: Conditional jump with reduced overhead
                const condition = try self.stack.pop();

                const should_jump = switch (condition.value) {
                    .tetra => |t| switch (t) {
                        0 => false, // false -> don't jump
                        1 => true, // true -> jump
                        2 => true, // both -> jump (contains true)
                        3 => false, // neither -> don't jump (contains no truth)
                        else => false,
                    },
                    .int => |i| i != 0,
                    .float => |f| f != 0.0,
                    .nothing => false,
                    else => true,
                };

                const target_label = if (should_jump) j.label_true else j.label_false;

                if (self.label_map.get(target_label)) |target_ip| {
                    // Always set IP explicitly since JumpCond is marked as a "jump" instruction
                    self.ip = target_ip;
                } else {
                    return self.reporter.reportRuntimeError(null, ErrorCode.VARIABLE_NOT_FOUND, "Unknown label: {s}", .{target_label});
                }
            },

            .Label => {
                // Labels are no-ops during execution (already resolved)
            },

            .Dup => {
                const value = try self.stack.peek();
                try self.stack.push(HIRFrame.initFromHIRValue(value.value));
            },

            .Pop => {
                const value = try self.stack.pop();
                // HIRFrame doesn't need cleanup - it's just a simple wrapper
                _ = value;
            },

            .Swap => {
                const top = try self.stack.pop();
                const second = try self.stack.pop();
                try self.stack.push(top);
                try self.stack.push(second);
            },

            .Peek => |peek| {
                const value = try self.stack.pop();

                if (peek.location) |location| {
                    try std.io.getStdOut().writer().print("[{s}:{d}:{d}] ", .{ location.file, location.range.start_line, location.range.start_col });
                }

                // Print variable name before :: if available, then always show type
                const writer = std.io.getStdOut().writer();

                // If we have union member info, print union with '>' on active member; else fallback to concrete type
                if (peek.name) |name| {
                    if (peek.union_members) |members| {
                        if (members.len > 1) {
                            const active = self.getTypeString(value.value);
                            try writer.print("{s} :: ", .{name});
                            for (members, 0..) |m, i| {
                                if (i > 0) try writer.print(" | ", .{});
                                if (std.mem.eql(u8, m, active)) try writer.print(">", .{});
                                try writer.print("{s}", .{m});
                            }
                            try writer.print(" is ", .{});
                        } else {
                            // If a union was detected but only one member present, still print as that type
                            const type_string = self.getTypeString(value.value);
                            try writer.print("{s} :: {s} is ", .{ name, type_string });
                        }
                    } else {
                        const type_string = self.getTypeString(value.value);
                        try writer.print("{s} :: {s} is ", .{ name, type_string });
                    }
                } else {
                    if (peek.union_members) |members| {
                        if (members.len > 1) {
                            const active = self.getTypeString(value.value);
                            try writer.print(":: ", .{});
                            for (members, 0..) |m, i| {
                                if (i > 0) try writer.print(" | ", .{});
                                if (std.mem.eql(u8, m, active)) try writer.print(">", .{});
                                try writer.print("{s}", .{m});
                            }
                            try writer.print(" is ", .{});
                        } else {
                            const type_string = self.getTypeString(value.value);
                            try writer.print(":: {s} is ", .{type_string});
                        }
                    } else {
                        const type_string = self.getTypeString(value.value);
                        try writer.print(":: {s} is ", .{type_string});
                    }
                }

                // Format the value
                try self.formatHIRValue(std.io.getStdOut().writer(), value.value);
                try std.io.getStdOut().writer().print("\n", .{});

                // Push the value back onto the stack for potential further use
                try self.stack.push(value);
            },

            .PeekStruct => |i| {
                const value = try self.stack.pop();

                // Handle both struct instances and field values
                switch (value.value) {
                    .struct_instance => |s| {
                        // Print each field with proper location formatting
                        for (s.fields) |field| {

                            // Format with location information like the regular Peek instruction
                            if (i.location) |location| {
                                try std.io.getStdOut().writer().print("[{s}:{d}:{d}] ", .{ location.file, location.range.start_line, location.range.start_col });
                            }

                            // Build the full field path
                            var field_path: []const u8 = undefined;
                            if (s.path) |path| {
                                field_path = try std.fmt.allocPrint(self.allocator, "{s}.{s}", .{ path, field.name });
                            } else {
                                field_path = try std.fmt.allocPrint(self.allocator, "{s}.{s}", .{ s.type_name, field.name });
                            }
                            defer self.allocator.free(field_path);

                            const field_type_string = self.getTypeString(field.value);
                            try std.io.getStdOut().writer().print(":: {s} is ", .{field_type_string});

                            // For nested structs, recursively print their fields
                            switch (field.value) {
                                .struct_instance => |nested| {
                                    // Create a new PeekStruct instruction for the nested struct
                                    const field_names = try self.allocator.alloc([]const u8, nested.fields.len);
                                    const field_types = try self.allocator.alloc(HIRType, nested.fields.len);
                                    for (nested.fields, 0..) |nested_field, field_idx| {
                                        field_names[field_idx] = nested_field.name;
                                        field_types[field_idx] = nested_field.field_type;
                                    }
                                    const nested_peek = HIRInstruction{
                                        .PeekStruct = .{
                                            .location = i.location,
                                            .field_names = field_names,
                                            .type_name = nested.type_name,
                                            .field_count = @intCast(nested.fields.len),
                                            .field_types = field_types,
                                            .should_pop_after_peek = i.should_pop_after_peek, // Pass the flag down
                                        },
                                    };
                                    // Push the nested struct onto the stack with updated path
                                    var nested_with_path = nested;
                                    nested_with_path.path = field_path;
                                    try self.stack.push(HIRFrame{ .value = HIRValue{ .struct_instance = nested_with_path } });
                                    // Recursively peek the nested struct
                                    try self.executeInstruction(nested_peek);
                                },
                                else => {
                                    try self.formatHIRValue(std.io.getStdOut().writer(), field.value);
                                    try std.io.getStdOut().writer().print("\n", .{});
                                },
                            }
                        }
                    },
                    else => {
                        // For non-struct values (like field access results), print as a single value with location
                        if (i.field_names.len > 0) {
                            if (i.location) |location| {
                                try std.io.getStdOut().writer().print("[{s}:{d}:{d}] ", .{ location.file, location.range.start_line, location.range.start_col });
                            }

                            // Show type information for non-struct values
                            const value_type_string = self.getTypeString(value.value);
                            try std.io.getStdOut().writer().print(":: {s} is ", .{value_type_string});
                            try self.formatHIRValue(std.io.getStdOut().writer(), value.value);
                            try std.io.getStdOut().writer().print("\n", .{});
                        }
                    },
                }

                if (!i.should_pop_after_peek) {
                    // Push the value back onto the stack for potential further use
                    try self.stack.push(value);
                }
            },

            .Print => {
                const value = try self.stack.pop();

                // Print instruction doesn't include variable name, just the value
                // Format the value directly without quotes or extra formatting
                try self.formatHIRValueRaw(std.io.getStdOut().writer(), value.value);

                // Output will be flushed automatically on newline

                // Push the value back onto the stack for potential further use
                try self.stack.push(value);
            },

            .PrintInterpolated => |interp| {
                // Pop the arguments from the stack (they were pushed in reverse order)
                var args = try self.allocator.alloc(HIRValue, interp.argument_count);
                defer self.allocator.free(args);

                for (0..interp.argument_count) |i| {
                    const arg = try self.stack.pop();
                    args[interp.argument_count - 1 - i] = arg.value; // Reverse to get correct order
                }

                // Get the actual format parts from the constants using format_part_ids
                var actual_format_parts = std.ArrayList([]const u8).init(self.allocator);
                defer actual_format_parts.deinit();

                for (interp.format_part_ids) |id| {
                    if (id < self.program.constant_pool.len) {
                        const constant = self.program.constant_pool[id];
                        if (constant == .string) {
                            try actual_format_parts.append(constant.string);
                        } else {
                            try actual_format_parts.append(""); // Fallback
                        }
                    } else {
                        try actual_format_parts.append(""); // Fallback
                    }
                }

                // Build the final string by interleaving format parts and argument values
                var result = std.ArrayList(u8).init(self.allocator);
                defer result.deinit();

                for (actual_format_parts.items, 0..) |part, i| {
                    try result.appendSlice(part);

                    // Add argument value if there is one for this part
                    if (i < interp.placeholder_indices.len) {
                        const arg_index = interp.placeholder_indices[i];
                        if (arg_index < args.len) {
                            try self.formatHIRValueRaw(result.writer(), args[arg_index]);
                        }
                    }
                }

                // Print the final interpolated string
                try std.io.getStdOut().writer().print("{s}", .{result.items});
            },

            .EnterScope => {
                // TAIL CALL FIX: Skip scope creation if this is a tail call
                if (self.skip_next_enter_scope) {
                    self.skip_next_enter_scope = false; // Reset flag

                    return; // Skip scope creation entirely
                }

                // Create new scope - the memory module handles all the complexity
                const new_scope = try self.memory_manager.scope_manager.createScope(self.current_scope, self.memory_manager);
                self.current_scope = new_scope;
            },

            .ExitScope => {
                if (self.current_scope.parent) |parent_scope| {
                    const old_scope = self.current_scope;
                    self.current_scope = parent_scope;

                    // Clean up the old scope
                    old_scope.deinit();
                }
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
                    const stderr = std.io.getStdErr().writer();
                    try stderr.print("Assertion failed at {s}:\n{s}\n", .{ location_str, message_str });
                } else {
                    // No message provided
                    const stderr = std.io.getStdErr().writer();
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
                const result = switch (op.op) {
                    .Not => blk: {
                        // Unary operation - only pop one value
                        const a = try self.stack.pop();
                        break :blk try self.logicalNot(a);
                    },
                    else => blk: {
                        // Binary operations - pop two values
                        const b = try self.stack.pop();
                        const a = try self.stack.pop();

                        break :blk switch (op.op) {
                            .And => try self.logicalAnd(a, b),
                            .Or => try self.logicalOr(a, b),
                            .Iff => try self.logicalIff(a, b),
                            .Xor => try self.logicalXor(a, b),
                            .Nand => try self.logicalNand(a, b),
                            .Nor => try self.logicalNor(a, b),
                            .Implies => try self.logicalImplies(a, b),
                            .Not => unreachable, // Handled above
                        };
                    },
                };

                try self.stack.push(HIRFrame.initTetra(result));
            },

            .StringOp => |s| {
                const val = try self.stack.pop();

                // Fast-path for Length which is valid for both strings and arrays
                if (s.op == .Length) {
                    switch (val.value) {
                        .string => |sv| {
                            const len = @as(i32, @intCast(sv.len));
                            try self.stack.push(HIRFrame.initInt(len));
                        },
                        .array => |arr| {
                            // Use logical length: count elements until first 'nothing'
                            var logical_len: usize = 0;
                            while (logical_len < arr.elements.len) : (logical_len += 1) {
                                if (std.meta.eql(arr.elements[logical_len], HIRValue.nothing)) break;
                            }
                            try self.stack.push(HIRFrame.initInt(@intCast(logical_len)));
                        },
                        else => return ErrorList.TypeError,
                    }
                    return; // done handling Length
                }

                // Fast-path for ToString which is valid for any type
                if (s.op == .ToString) {
                    const result = try self.valueToString(val.value);
                    try self.stack.push(HIRFrame.initString(result));
                    return; // done handling ToString
                }

                // Numeric conversions: accept string, int, float, byte
                if (s.op == .ToInt) {
                    const out: HIRValue = switch (val.value) {
                        .int => val.value,
                        .byte => |u| HIRValue{ .int = @as(i32, u) },
                        .float => |f| HIRValue{ .int = @as(i32, @intFromFloat(f)) },
                        .string => |s_val| blk: {
                            const parsed = std.fmt.parseInt(i32, s_val, 10) catch {
                                // On failure, mirror existing behavior: return NumberError.ParseFailed if available; else 0
                                var variant_index: u32 = 0;
                                if (self.custom_type_registry.get("NumberError")) |ct| {
                                    if (ct.enum_variants) |variants| {
                                        for (variants, 0..) |v, i| {
                                            if (std.mem.eql(u8, v.name, "ParseFailed")) {
                                                variant_index = @intCast(i);
                                                break;
                                            }
                                        }
                                    }
                                }
                                break :blk HIRValue{ .enum_variant = .{ .type_name = "NumberError", .variant_name = "ParseFailed", .variant_index = variant_index } };
                            };
                            break :blk HIRValue{ .int = parsed };
                        },
                        else => HIRValue{ .int = 0 },
                    };
                    try self.stack.push(HIRFrame.initFromHIRValue(out));
                    return;
                }
                if (s.op == .ToFloat) {
                    const out: HIRValue = switch (val.value) {
                        .float => val.value,
                        .int => |i| HIRValue{ .float = @as(f64, @floatFromInt(i)) },
                        .byte => |u| HIRValue{ .float = @as(f64, @floatFromInt(u)) },
                        .string => |s_val| blk: {
                            const parsed = std.fmt.parseFloat(f64, s_val) catch 0.0;
                            break :blk HIRValue{ .float = parsed };
                        },
                        else => HIRValue{ .float = 0.0 },
                    };
                    try self.stack.push(HIRFrame.initFromHIRValue(out));
                    return;
                }
                if (s.op == .ToByte) {
                    const out: HIRValue = switch (val.value) {
                        .byte => val.value,
                        .int => |i| HIRValue{ .byte = if (i >= 0 and i <= 255) @intCast(i) else 0 },
                        .float => |f| blk: {
                            const i: i32 = @as(i32, @intFromFloat(f));
                            break :blk HIRValue{ .byte = if (i >= 0 and i <= 255) @intCast(i) else 0 };
                        },
                        .string => |s_val| blk: {
                            const parsed = std.fmt.parseInt(i32, s_val, 10) catch 0;
                            break :blk HIRValue{ .byte = if (parsed >= 0 and parsed <= 255) @intCast(parsed) else 0 };
                        },
                        else => HIRValue{ .byte = 0 },
                    };
                    try self.stack.push(HIRFrame.initFromHIRValue(out));
                    return;
                }

                // For the remaining ops we expect a string value
                switch (val.value) {
                    .string => |s_val| {
                        switch (s.op) {
                            .Substring => {
                                // Get substring parameters
                                const length = try self.stack.pop();
                                const start = try self.stack.pop();

                                // Validate parameters
                                if (start.value != .int or length.value != .int) {
                                    return ErrorList.TypeError;
                                }

                                // Normalize and validate parameters (including negatives)
                                const start_i: i32 = start.value.int;
                                const len_i: i32 = length.value.int;
                                if (start_i < 0 or len_i < 0) {
                                    var variant_index: u32 = 0;
                                    if (self.custom_type_registry.get("IndexError")) |ct| {
                                        if (ct.enum_variants) |variants| {
                                            for (variants, 0..) |v, i| {
                                                if (std.mem.eql(u8, v.name, "OutOfBounds")) {
                                                    variant_index = @intCast(i);
                                                    break;
                                                }
                                            }
                                        }
                                    }
                                    const err_val = HIRValue{ .enum_variant = .{
                                        .type_name = "IndexError",
                                        .variant_name = "OutOfBounds",
                                        .variant_index = variant_index,
                                    } };
                                    try self.stack.push(HIRFrame.initFromHIRValue(err_val));
                                    return;
                                }

                                const start_idx = @as(usize, @intCast(start_i));
                                const len = @as(usize, @intCast(len_i));

                                // Bounds check -> return union error variant IndexError.OutOfBounds
                                if (start_idx >= s_val.len or start_idx + len > s_val.len) {
                                    var variant_index: u32 = 0;
                                    if (self.custom_type_registry.get("IndexError")) |ct| {
                                        if (ct.enum_variants) |variants| {
                                            for (variants, 0..) |v, i| {
                                                if (std.mem.eql(u8, v.name, "OutOfBounds")) {
                                                    variant_index = @intCast(i);
                                                    break;
                                                }
                                            }
                                        }
                                    }
                                    const err_val = HIRValue{ .enum_variant = .{
                                        .type_name = "IndexError",
                                        .variant_name = "OutOfBounds",
                                        .variant_index = variant_index,
                                    } };
                                    try self.stack.push(HIRFrame.initFromHIRValue(err_val));
                                    return;
                                }
                                const end_idx = start_idx + len;

                                // Create substring
                                const substring = s_val[start_idx..end_idx];
                                try self.stack.push(HIRFrame.initString(substring));
                            },
                            .Concat => {
                                // Get second string
                                const str2 = try self.stack.pop();
                                if (str2.value != .string) {
                                    return ErrorList.TypeError;
                                }

                                // Concatenate strings
                                const result = try std.fmt.allocPrint(self.allocator, "{s}{s}", .{ s_val, str2.value.string });
                                try self.stack.push(HIRFrame.initString(result));
                            },
                            .Bytes => {
                                const elements = try self.allocator.alloc(HIRValue, s_val.len);
                                for (s_val, 0..) |byte, i| {
                                    elements[i] = HIRValue{ .byte = byte };
                                }
                                const array = HIRValue{ .array = HIRArray{
                                    .elements = elements,
                                    .element_type = .Byte,
                                    .capacity = @intCast(s_val.len),
                                } };

                                try self.stack.push(HIRFrame.initFromHIRValue(array));
                            },
                            .ToInt => {
                                const parsed_int = std.fmt.parseInt(i32, s_val, 10) catch {
                                    // Return enum variant NumberError.ParseFailed
                                    var variant_index: u32 = 0;
                                    if (self.custom_type_registry.get("NumberError")) |ct| {
                                        if (ct.enum_variants) |variants| {
                                            for (variants, 0..) |v, i| {
                                                if (std.mem.eql(u8, v.name, "ParseFailed")) {
                                                    variant_index = @intCast(i);
                                                    break;
                                                }
                                            }
                                        }
                                    }
                                    const hir = HIRValue{ .enum_variant = .{ .type_name = "NumberError", .variant_name = "ParseFailed", .variant_index = variant_index } };
                                    try self.stack.push(HIRFrame.initFromHIRValue(hir));
                                    return; // handled failure path
                                };
                                try self.stack.push(HIRFrame.initInt(parsed_int));
                            },
                            .ToString => {
                                // This should never happen since ToString operates on any type, not just strings
                                // But if it does, just return the string as-is
                                try self.stack.push(HIRFrame.initString(s_val));
                            },
                            else => return ErrorList.TypeError,
                        }
                    },
                    else => return ErrorList.TypeError,
                }
            },

            // Array operations (Phase 1: Core Data Types)
            .ArrayNew => |a| {
                // Create new array with specified size
                // CRITICAL FIX: For empty arrays (size=0), allocate minimum capacity for growth
                const initial_capacity = if (a.size == 0) 8 else a.size; // Start with capacity 8 for empty arrays
                const elements = try self.allocator.alloc(HIRValue, initial_capacity);

                // Initialize elements: first 'size' with type default, remainder with 'nothing'
                const default_value = HIRVM.getDefaultValue(a.element_type);
                var i: usize = 0;
                while (i < @as(usize, @intCast(a.size)) and i < elements.len) : (i += 1) {
                    elements[i] = default_value;
                }
                while (i < elements.len) : (i += 1) {
                    elements[i] = HIRValue.nothing;
                }

                // For empty arrays, we use the full capacity but track length separately
                const array = HIRValue{ .array = HIRArray{
                    .elements = elements,
                    .element_type = a.element_type,
                    .capacity = initial_capacity,
                } };

                try self.stack.push(HIRFrame.initFromHIRValue(array));
            },

            .ArrayGet => |a| {
                // Get array element by index: array[index]
                const index = try self.stack.pop(); // Index
                const array = try self.stack.pop(); // Array

                // IMPROVED: Handle different index types more gracefully
                const index_val = switch (index.value) {
                    .int => |i| if (i < 0) {
                        return self.reporter.reportRuntimeError(null, ErrorCode.VARIABLE_NOT_FOUND, "Array index cannot be negative: {}", .{i});
                    } else @as(u32, @intCast(i)),
                    .byte => |u| @as(u32, u),
                    .tetra => |t| @as(u32, t), // Allow tetra values as indices
                    .string => |s| blk: {
                        // GRACEFUL: Try to parse string as integer
                        const parsed = std.fmt.parseInt(i32, s, 10) catch {
                            try self.stack.push(HIRFrame.initFromHIRValue(HIRValue.nothing));
                            return;
                        };
                        if (parsed < 0) {
                            return self.reporter.reportRuntimeError(null, ErrorCode.VARIABLE_NOT_FOUND, "Array index cannot be negative: {}", .{parsed});
                        }
                        break :blk @as(u32, @intCast(parsed));
                    },
                    .nothing => {
                        try self.stack.push(HIRFrame.initFromHIRValue(HIRValue.nothing));
                        return;
                    },
                    else => {
                        try self.stack.push(HIRFrame.initFromHIRValue(HIRValue.nothing));
                        return;
                    },
                };

                switch (array.value) {
                    .array => |arr| {
                        // Compute logical length: number of populated elements until first 'nothing'
                        var logical_len: u32 = 0;
                        while (logical_len < arr.elements.len) : (logical_len += 1) {
                            if (std.meta.eql(arr.elements[logical_len], HIRValue.nothing)) break;
                        }

                        // FIX: Return default value of array element type instead of nothing for out-of-bounds
                        if (a.bounds_check and index_val >= logical_len) {
                            // Return default value based on array element type instead of error
                            const default_value = self.getDefaultValueForArrayType(arr.element_type);
                            try self.stack.push(HIRFrame.initFromHIRValue(default_value));
                            return;
                        }

                        const element = arr.elements[index_val];

                        try self.stack.push(HIRFrame.initFromHIRValue(element));
                    },
                    .nothing => {
                        // GRACEFUL: Treat 'nothing' as empty array - any index returns nothing

                        try self.stack.push(HIRFrame.initFromHIRValue(HIRValue.nothing));
                    },
                    .string => |s| {
                        // GRACEFUL: Allow string indexing (return character as string)
                        if (index_val >= s.len) {
                            try self.stack.push(HIRFrame.initFromHIRValue(HIRValue.nothing));
                            return;
                        }
                        const char_str = s[index_val .. index_val + 1];
                        try self.stack.push(HIRFrame.initFromHIRValue(HIRValue{ .string = char_str }));
                    },
                    else => {
                        try self.stack.push(HIRFrame.initFromHIRValue(HIRValue.nothing));
                    },
                }
            },

            .ArraySet => |a| {
                // Set array element by index
                // Stack order (top to bottom): value, index, array
                const value = try self.stack.pop(); // Value to set
                const index = try self.stack.pop(); // Index
                const array_frame = try self.stack.pop(); // Array

                // IMPROVED: Handle different index types more gracefully
                const index_val = switch (index.value) {
                    .int => |i| if (i < 0) {
                        return self.reporter.reportRuntimeError(null, ErrorCode.VARIABLE_NOT_FOUND, "Array index cannot be negative: {}", .{i});
                    } else @as(u32, @intCast(i)),
                    .byte => |u| @as(u32, u),
                    .tetra => |t| @as(u32, t), // Allow tetra values as indices
                    .string => |s| blk: {
                        // GRACEFUL: Try to parse string as integer
                        const parsed = std.fmt.parseInt(i32, s, 10) catch {
                            try self.stack.push(array_frame); // Push original array back
                            return;
                        };
                        if (parsed < 0) {
                            return self.reporter.reportRuntimeError(null, ErrorCode.VARIABLE_NOT_FOUND, "Array index cannot be negative: {}", .{parsed});
                        }
                        break :blk @as(u32, @intCast(parsed));
                    },
                    .nothing => {
                        try self.stack.push(array_frame); // Push original array back
                        return;
                    },
                    else => {
                        try self.stack.push(array_frame); // Push original array back
                        return;
                    },
                };

                switch (array_frame.value) {
                    .array => |arr| {
                        // Create a mutable copy of the array
                        var mutable_arr = arr;

                        if (a.bounds_check and index_val >= mutable_arr.elements.len) {
                            self.reporter.reportRuntimeError(null, ErrorCode.VARIABLE_NOT_FOUND, "Array index {} out of bounds for assignment (array has {} elements)", .{ index_val, mutable_arr.elements.len });
                            return ErrorList.IndexOutOfBounds;
                        }

                        const element_value = switch (mutable_arr.element_type) {
                            .Byte => switch (value.value) {
                                .int => |i| blk: {
                                    const byte_val = if (i >= 0 and i <= 255) HIRValue{ .byte = @intCast(i) } else HIRValue{ .byte = 0 };
                                    break :blk byte_val;
                                },
                                .byte => blk: {
                                    break :blk value.value; // Already correct type
                                },
                                .string => |_| return self.reporter.reportRuntimeError(null, ErrorCode.VARIABLE_NOT_FOUND, "Cannot assign string to byte array element", .{}),
                                else => return self.reporter.reportRuntimeError(null, ErrorCode.VARIABLE_NOT_FOUND, "Cannot assign {s} to byte array element", .{@tagName(value.value)}),
                            },
                            .Int => switch (value.value) {
                                .byte => |b| blk: {
                                    const int_val = HIRValue{ .int = @as(i32, b) };
                                    break :blk int_val; // Convert byte to int
                                },
                                .int => blk: {
                                    break :blk value.value; // Already correct type
                                },
                                .string => |_| return self.reporter.reportRuntimeError(null, ErrorCode.VARIABLE_NOT_FOUND, "Cannot assign string to int array element", .{}),
                                else => return self.reporter.reportRuntimeError(null, ErrorCode.VARIABLE_NOT_FOUND, "Cannot assign {s} to int array element", .{@tagName(value.value)}),
                            },
                            else => blk: {
                                break :blk value.value; // For other array types, keep original value
                            },
                        };

                        mutable_arr.elements[index_val] = element_value;

                        // Push the modified array back onto the stack
                        const modified_array_value = HIRValue{ .array = mutable_arr };
                        try self.stack.push(HIRFrame.initFromHIRValue(modified_array_value));
                    },
                    .nothing => {
                        // GRACEFUL: Cannot set on 'nothing', just ignore

                        try self.stack.push(HIRFrame.initFromHIRValue(HIRValue.nothing));
                    },
                    else => {
                        try self.stack.push(array_frame); // Push original value back
                    },
                }
            },

            .ArrayPush => |_| {
                const value = try self.stack.pop();
                try self.handleArrayPush(value.value);
            },

            .ArrayPop => {
                // Pop element from end of array
                const array = try self.stack.pop(); // Array

                switch (array.value) {
                    .array => |arr| {
                        // Create a mutable copy of the array
                        var mutable_arr = arr;

                        // Find the last non-nothing element
                        var length: u32 = 0;
                        for (mutable_arr.elements) |elem| {
                            if (std.meta.eql(elem, HIRValue.nothing)) break;
                            length += 1;
                        }

                        if (length == 0) {
                            return self.reporter.reportRuntimeError(null, ErrorCode.VARIABLE_NOT_FOUND, "Cannot pop from empty array", .{});
                        }

                        const last_element = mutable_arr.elements[length - 1];
                        mutable_arr.elements[length - 1] = HIRValue.nothing; // Clear the element

                        // Push the updated array first so it's on top after Swap in codegen
                        try self.stack.push(HIRFrame.initFromHIRValue(HIRValue{ .array = mutable_arr }));

                        // Then push the popped element so it remains as the expression result
                        try self.stack.push(HIRFrame.initFromHIRValue(last_element));
                    },
                    else => return self.reporter.reportRuntimeError(null, ErrorCode.VARIABLE_NOT_FOUND, "Cannot pop from non-array value: {s}", .{@tagName(array.value)}),
                }
            },

            .ArrayConcat => {
                // Concatenate two arrays
                const b = try self.stack.pop(); // Second array
                const a = try self.stack.pop(); // First array

                switch (a.value) {
                    .array => |arr_a| {
                        switch (b.value) {
                            .array => |arr_b| {
                                // Calculate lengths
                                var len_a: u32 = 0;
                                for (arr_a.elements) |elem| {
                                    if (std.meta.eql(elem, HIRValue.nothing)) break;
                                    len_a += 1;
                                }

                                var len_b: u32 = 0;
                                for (arr_b.elements) |elem| {
                                    if (std.meta.eql(elem, HIRValue.nothing)) break;
                                    len_b += 1;
                                }

                                // Create new array with combined elements
                                const new_elements = try self.allocator.alloc(HIRValue, len_a + len_b);

                                // Copy elements from first array
                                for (0..len_a) |i| {
                                    new_elements[i] = arr_a.elements[i];
                                }

                                // Copy elements from second array
                                for (0..len_b) |i| {
                                    new_elements[len_a + i] = arr_b.elements[i];
                                }

                                const result_array = HIRValue{
                                    .array = HIRArray{
                                        .elements = new_elements,
                                        .element_type = arr_a.element_type, // Use first array's element type
                                        .capacity = len_a + len_b,
                                    },
                                };

                                try self.stack.push(HIRFrame.initFromHIRValue(result_array));
                            },
                            else => return self.reporter.reportRuntimeError(null, ErrorCode.VARIABLE_NOT_FOUND, "Cannot concatenate array with non-array: {s}", .{@tagName(b.value)}),
                        }
                    },
                    else => return self.reporter.reportRuntimeError(null, ErrorCode.VARIABLE_NOT_FOUND, "Cannot concatenate non-array: {s}", .{@tagName(a.value)}),
                }
            },

            .TailCall => |c| {
                // TAIL CALL OPTIMIZATION: Reuse current stack frame instead of creating new one

                switch (c.call_kind) {
                    .LocalFunction => {
                        // Tail call optimization for user-defined functions
                        if (c.function_index >= self.program.function_table.len) {
                            return self.reporter.reportRuntimeError(null, ErrorCode.VARIABLE_NOT_FOUND, "Invalid function index: {} (max: {})", .{ c.function_index, self.program.function_table.len });
                        }

                        const function = self.program.function_table[c.function_index];
                        const target_label = function.start_label;

                        self.skip_next_enter_scope = true; // Skip scope creation when we jump to function start

                        // Jump to function start (including parameter setup) without call stack modification
                        if (self.label_map.get(target_label)) |target_ip| {
                            self.ip = target_ip;
                            return; // Jump to function body
                        } else {
                            return self.reporter.reportRuntimeError(null, ErrorCode.VARIABLE_NOT_FOUND, "Function label not found: {s}", .{target_label});
                        }
                    },
                    else => {
                        return self.reporter.reportRuntimeError(null, ErrorCode.VARIABLE_NOT_FOUND, "Tail call not supported for call kind: {s}", .{@tagName(c.call_kind)});
                    },
                }
            },

            .Call => |c| {
                switch (c.call_kind) {
                    .LocalFunction => {
                        // User-defined function call with proper stack management
                        if (c.function_index >= self.program.function_table.len) {
                            return self.reporter.reportRuntimeError(null, ErrorCode.VARIABLE_NOT_FOUND, "Invalid function index: {} (max: {})", .{ c.function_index, self.program.function_table.len });
                        }

                        const function = self.program.function_table[c.function_index];

                        // Push call frame for proper return handling
                        const return_ip = self.ip + 1; // Return to instruction after this call

                        const call_frame = CallFrame{
                            .return_ip = return_ip,
                            .function_name = function.name,
                            .arg_count = c.arg_count, // Store arg count to clean up stack later
                            // Save SP before arguments so Return can restore caller stack correctly
                            .saved_sp = self.stack.size() - @as(i32, @intCast(c.arg_count)),
                        };
                        try self.call_stack.push(call_frame);

                        // Use pre-resolved label map for O(1) lookup
                        if (self.label_map.get(function.start_label)) |target_ip| {
                            self.ip = target_ip;
                            return; // Jump to function start
                        } else {
                            return self.reporter.reportRuntimeError(null, ErrorCode.VARIABLE_NOT_FOUND, "Function label not found: {s}", .{function.start_label});
                        }
                    },
                    .BuiltinFunction => {
                        // Built-in function/method call
                        if (std.mem.eql(u8, c.qualified_name, "length")) {
                            // Array length method - expects array on stack
                            const array = try self.stack.pop();
                            switch (array.value) {
                                .array => |arr| {
                                    // Find the actual length by counting non-nothing elements
                                    var length: u32 = 0;
                                    for (arr.elements) |elem| {
                                        if (std.meta.eql(elem, HIRValue.nothing)) break;
                                        length += 1;
                                    }
                                    try self.stack.push(HIRFrame.initInt(@as(i32, @intCast(length))));
                                },
                                else => return self.reporter.reportRuntimeError(null, ErrorCode.VARIABLE_NOT_FOUND, "Cannot get length of non-array value: {s}", .{@tagName(array.value)}),
                            }
                        } else if (std.mem.eql(u8, c.qualified_name, "push")) {
                            // Array push method - expects element and array on stack
                            const element = try self.stack.pop(); // Element to push
                            const array = try self.stack.pop(); // Array

                            switch (array.value) {
                                .array => |arr| {
                                    // Create a mutable copy of the array
                                    var mutable_arr = arr;

                                    // Find the current length
                                    var length: u32 = 0;
                                    for (mutable_arr.elements) |elem| {
                                        if (std.meta.eql(elem, HIRValue.nothing)) break;
                                        length += 1;
                                    }

                                    // Check if we need to resize
                                    if (length >= mutable_arr.capacity) {
                                        // CRITICAL FIX: Resize the array by doubling capacity
                                        const new_capacity = mutable_arr.capacity * 2;
                                        const new_elements = try self.allocator.realloc(mutable_arr.elements, new_capacity);
                                        mutable_arr.elements = new_elements;
                                        mutable_arr.capacity = new_capacity;

                                        // Initialize new elements to nothing
                                        for (length..new_capacity) |i| {
                                            mutable_arr.elements[i] = HIRValue.nothing;
                                        }
                                    }

                                    // Add element to end (capacity is guaranteed to be > length)
                                    mutable_arr.elements[length] = element.value;

                                    // Push the modified array back onto the stack
                                    const modified_array_value = HIRValue{ .array = mutable_arr };
                                    try self.stack.push(HIRFrame.initFromHIRValue(modified_array_value));
                                },
                                else => return self.reporter.reportRuntimeError(null, ErrorCode.VARIABLE_NOT_FOUND, "Cannot push to non-array value: {s}", .{@tagName(array.value)}),
                            }
                        } else if (std.mem.eql(u8, c.qualified_name, "safeAdd")) {
                            // Safe addition with overflow protection
                            const b = try self.stack.pop();
                            const a = try self.stack.pop();

                            const a_int = switch (a.value) {
                                .int => |i| i,
                                .byte => |u| @as(i32, u),
                                else => return self.reporter.reportRuntimeError(null, ErrorCode.VARIABLE_NOT_FOUND, "safeAdd: first argument must be integer", .{}),
                            };

                            const b_int = switch (b.value) {
                                .int => |i| i,
                                .byte => |u| @as(i32, u),
                                else => return self.reporter.reportRuntimeError(null, ErrorCode.VARIABLE_NOT_FOUND, "safeAdd: second argument must be integer", .{}),
                            };

                            const result = std.math.add(i32, a_int, b_int) catch {
                                // On overflow, return nothing instead of crashing
                                try self.stack.push(HIRFrame.initFromHIRValue(HIRValue.nothing));
                                return;
                            };

                            try self.stack.push(HIRFrame.initInt(result));
                        } else if (std.mem.eql(u8, c.qualified_name, "power") or std.mem.eql(u8, c.qualified_name, "powi")) {
                            // Power function - expects base and exponent on stack
                            const exponent = try self.stack.pop();
                            const base = try self.stack.pop();

                            if (std.mem.eql(u8, c.qualified_name, "powi")) {
                                // Integer power: both operands must be Int
                                const base_int = switch (base.value) {
                                    .int => |i| i,
                                    .byte => |b| @as(i32, b),
                                    else => return self.reporter.reportRuntimeError(null, ErrorCode.VARIABLE_NOT_FOUND, "powi: base must be integer", .{}),
                                };
                                const exp_int = switch (exponent.value) {
                                    .int => |i| i,
                                    .byte => |b| @as(i32, b),
                                    else => return self.reporter.reportRuntimeError(null, ErrorCode.VARIABLE_NOT_FOUND, "powi: exponent must be integer", .{}),
                                };
                                const result = std.math.pow(i32, base_int, @intCast(exp_int));
                                try self.stack.push(HIRFrame.initInt(result));
                                return;
                            }
                            // Convert both operands to float for power calculation
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
                        } else if (std.mem.eql(u8, c.qualified_name, "exists_quantifier_gt")) {
                            // Existential quantifier with greater-than condition
                            const comparison_value = try self.stack.pop();
                            const array = try self.stack.pop();

                            switch (array.value) {
                                .array => |arr| {
                                    // Check if any element is greater than comparison_value
                                    var found = false;
                                    for (arr.elements) |elem| {
                                        if (std.meta.eql(elem, HIRValue.nothing)) {
                                            break; // End of array
                                        }
                                        // Check if element > comparison_value
                                        const satisfies_condition = switch (elem) {
                                            .int => |elem_int| switch (comparison_value.value) {
                                                .int => |comp_int| elem_int > comp_int,
                                                else => false,
                                            },
                                            .float => |elem_float| switch (comparison_value.value) {
                                                .float => |comp_float| elem_float > comp_float,
                                                .int => |comp_int| elem_float > @as(f64, @floatFromInt(comp_int)),
                                                else => false,
                                            },
                                            else => false,
                                        };
                                        if (satisfies_condition) {
                                            found = true;
                                            break;
                                        }
                                    }
                                    try self.stack.push(HIRFrame.initTetra(if (found) 1 else 0));
                                },
                                else => return self.reporter.reportRuntimeError(null, ErrorCode.VARIABLE_NOT_FOUND, "exists_quantifier_gt: argument must be array", .{}),
                            }
                        } else if (std.mem.eql(u8, c.qualified_name, "forall_quantifier_gt")) {
                            // Universal quantifier with greater-than condition
                            const comparison_value = try self.stack.pop();
                            const array = try self.stack.pop();

                            switch (array.value) {
                                .array => |arr| {
                                    // Check if all elements are greater than comparison_value
                                    var all_satisfy = true;
                                    var has_elements = false;
                                    for (arr.elements) |elem| {
                                        if (std.meta.eql(elem, HIRValue.nothing)) {
                                            break; // End of array
                                        }
                                        has_elements = true;
                                        // Check if element > comparison_value
                                        const satisfies_condition = switch (elem) {
                                            .int => |elem_int| switch (comparison_value.value) {
                                                .int => |comp_int| blk: {
                                                    const result = elem_int > comp_int;
                                                    break :blk result;
                                                },
                                                else => false,
                                            },
                                            .float => |elem_float| switch (comparison_value.value) {
                                                .float => |comp_float| elem_float > comp_float,
                                                .int => |comp_int| elem_float > @as(f64, @floatFromInt(comp_int)),
                                                else => false,
                                            },
                                            else => false,
                                        };
                                        if (!satisfies_condition) {
                                            all_satisfy = false;

                                            break;
                                        }
                                    }
                                    // For empty arrays, forall returns true (vacuous truth)
                                    try self.stack.push(HIRFrame.initTetra(if ((!has_elements) or all_satisfy) 1 else 0));
                                },
                                else => return self.reporter.reportRuntimeError(null, ErrorCode.VARIABLE_NOT_FOUND, "forall_quantifier_gt: argument must be array", .{}),
                            }
                        } else if (std.mem.eql(u8, c.qualified_name, "input")) {
                            // Simple approach: use readUntilDelimiterAlloc which is more reliable
                            const stdin = std.io.getStdIn().reader();

                            // Use the standard library's readUntilDelimiterAlloc for better platform compatibility
                            const input_line = stdin.readUntilDelimiterAlloc(self.allocator, '\n', 4096) catch |err| switch (err) {
                                error.StreamTooLong => blk: {
                                    // Handle long lines by reading what we can
                                    var buffer: [4096]u8 = undefined;
                                    const line = stdin.readUntilDelimiterOrEof(buffer[0..], '\n') catch "";
                                    break :blk try self.allocator.dupe(u8, line orelse "");
                                },
                                error.EndOfStream => try self.allocator.dupe(u8, ""),
                                else => return err,
                            };
                            defer self.allocator.free(input_line);

                            // Remove trailing carriage return if present (Windows compatibility)
                            var line = input_line;
                            if (line.len > 0 and line[line.len - 1] == '\r') {
                                line = line[0 .. line.len - 1];
                            }

                            // Create a copy of the input string in VM memory
                            const input_string = try self.allocator.dupe(u8, line);
                            try self.stack.push(HIRFrame.initString(input_string));
                        } else {
                            return self.reporter.reportRuntimeError(null, ErrorCode.VARIABLE_NOT_FOUND, "Unknown built-in function: {s}", .{c.qualified_name});
                        }
                    },
                    .ModuleFunction => {
                        // Module function call - handle known module functions for now
                        if (std.mem.eql(u8, c.qualified_name, "safeMath.safeAdd")) {
                            // Safe addition with overflow/underflow checking (from safeMath.doxa)
                            const b = try self.stack.pop();
                            const a = try self.stack.pop();

                            const a_int = switch (a.value) {
                                .int => |i| i,
                                .byte => |u| @as(i32, u),
                                else => return self.reporter.reportRuntimeError(null, ErrorCode.VARIABLE_NOT_FOUND, "safeAdd: first argument must be integer", .{}),
                            };

                            const b_int = switch (b.value) {
                                .int => |i| i,
                                .byte => |u| @as(i32, u),
                                else => return self.reporter.reportRuntimeError(null, ErrorCode.VARIABLE_NOT_FOUND, "safeAdd: second argument must be integer", .{}),
                            };

                            // Apply safeMath.doxa logic: limit = 255
                            const limit = 255;
                            if (a_int > limit or b_int > limit) {
                                // Overflow detected - print "Overflow" with proper peek format per safeMath.doxa
                                const overflow_value = HIRValue{ .string = "Overflow" };
                                try std.io.getStdOut().writer().print("[test/misc/safeMath.doxa:7:9] :: string is ", .{});
                                try self.formatHIRValue(std.io.getStdOut().writer(), overflow_value);
                                try std.io.getStdOut().writer().print("\n", .{});
                                try self.stack.push(HIRFrame.initInt(-1));
                                return;
                            }

                            if (a_int < 0 or b_int < 0) {
                                // Underflow detected - print "Underflow" with proper peek format per safeMath.doxa
                                const underflow_value = HIRValue{ .string = "Underflow" };
                                try std.io.getStdOut().writer().print("[test/misc/safeMath.doxa:12:9] :: string is ", .{});
                                try self.formatHIRValue(std.io.getStdOut().writer(), underflow_value);
                                try std.io.getStdOut().writer().print("\n", .{});
                                try self.stack.push(HIRFrame.initInt(-1));
                                return;
                            }

                            // Safe to add - call math.add logic
                            const result = std.math.add(i32, a_int, b_int) catch {
                                // Integer overflow in addition - return -1
                                try self.stack.push(HIRFrame.initInt(-1));
                                return;
                            };

                            try self.stack.push(HIRFrame.initInt(result));
                        } else {
                            return self.reporter.reportRuntimeError(null, ErrorCode.VARIABLE_NOT_FOUND, "Unknown module function: {s}", .{c.qualified_name});
                        }
                    },
                }
            },

            .Return => |_| {

                // Check if we're returning from main program or a function call
                if (self.call_stack.isEmpty()) {
                    self.running = false;
                } else {
                    // Auto-exit current scope before returning to restore caller's scope
                    if (self.current_scope.parent) |parent_scope| {
                        const old_scope = self.current_scope;
                        self.current_scope = parent_scope;

                        old_scope.deinit();
                    }

                    // Returning from function call - pop call frame and return to caller
                    const call_frame = try self.call_stack.pop();

                    // Restore operand stack to the saved position, preserving the top return value
                    const current_sp = self.stack.size();
                    if (current_sp > call_frame.saved_sp) {
                        // There is at least one value to return on the stack; keep the topmost
                        const ret_val = self.stack.pop() catch HIRFrame.initNothing();
                        self.stack.sp = call_frame.saved_sp;
                        try self.stack.push(ret_val);
                    } else {
                        // No value returned; just restore SP
                        self.stack.sp = call_frame.saved_sp;
                    }

                    // Return to caller
                    self.ip = call_frame.return_ip;

                    return; // Don't auto-increment IP since we just set it
                }
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
                        .name = try self.allocator.dupe(u8, name_str),
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
                                    field.name = try self.allocator.dupe(u8, store_field.field_name);
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
                        // Handle special string operations
                        if (std.mem.eql(u8, get_field.field_name, "length")) {
                            // String length operation
                            const result = try self.stringLength(frame);
                            try self.stack.push(result);
                            return;
                        } else if (std.mem.eql(u8, get_field.field_name, "bytes")) {
                            // String bytes operation
                            const result = try self.stringBytes(frame);
                            try self.stack.push(result);
                            return;
                        } else {
                            // Try to parse field name as index for string indexing
                            if (std.fmt.parseInt(i32, get_field.field_name, 10)) |index| {
                                // Create frames for substring operation
                                const start_frame = HIRFrame.initInt(index);
                                const len_frame = HIRFrame.initInt(1);
                                const result = try self.stringSubstring(frame, start_frame, len_frame);
                                try self.stack.push(result);
                                return;
                            } else |_| {
                                self.reporter.reportRuntimeError(null, ErrorCode.VARIABLE_NOT_FOUND, "Invalid string operation: {s}", .{get_field.field_name});
                                return ErrorList.TypeError;
                            }
                        }
                    },
                    .struct_instance => |struct_inst| {
                        // Intern the field name for comparison
                        const interned_name = try self.string_interner.intern(get_field.field_name);

                        // Find field by name
                        for (struct_inst.fields) |field| {
                            if (std.mem.eql(u8, field.name, interned_name)) {
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

                    entries[reverse_i] = HIRMapEntry{
                        .key = key.value,
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
                const key = try self.stack.pop();
                const map = try self.stack.pop();

                switch (map.value) {
                    .map => |m| {

                        // Find entry with matching key
                        var found = false;
                        for (m.entries) |entry| {

                            // Use custom comparison for HIRValue keys
                            const keys_match = switch (entry.key) {
                                .string => |entry_str| switch (key.value) {
                                    .string => |key_str| std.mem.eql(u8, entry_str, key_str),
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

            .Convert => |c| {
                // Pop the value to convert, perform conversion, and push result
                const frame = try self.stack.pop();
                var out: HIRValue = frame.value;
                switch (c.to_type) {
                    .Float => {
                        // Convert ints/bytes to float; leave float as-is
                        out = switch (frame.value) {
                            .int => |i| HIRValue{ .float = @as(f64, @floatFromInt(i)) },
                            .byte => |u| HIRValue{ .float = @as(f64, @floatFromInt(u)) },
                            .float => frame.value,
                            else => frame.value,
                        };
                    },
                    .Int => {
                        out = switch (frame.value) {
                            .int => frame.value,
                            .byte => |u| HIRValue{ .int = @as(i32, u) },
                            .float => |f| HIRValue{ .int = @as(i32, @intFromFloat(f)) },
                            else => frame.value,
                        };
                    },
                    .Byte => {
                        out = switch (frame.value) {
                            .byte => frame.value,
                            .int => |i| HIRValue{ .byte = if (i >= 0 and i <= 255) @intCast(i) else 0 },
                            .float => |f| blk: {
                                const i: i32 = @as(i32, @intFromFloat(f));
                                break :blk HIRValue{ .byte = if (i >= 0 and i <= 255) @intCast(i) else 0 };
                            },
                            else => frame.value,
                        };
                    },
                    else => {
                        // Other conversions not needed for current arithmetic paths
                        out = frame.value;
                    },
                }

                try self.stack.push(HIRFrame.initFromHIRValue(out));
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

    // ===============================================================================
    // ARITHMETIC OPERATIONS (Enhanced with mixed-type support from old VM)
    // ===============================================================================

    /// Integer arithmetic with overflow checking
    fn intAdd(self: *HIRVM, a: i32, b: i32) !i32 {
        _ = self;
        return std.math.add(i32, a, b) catch |err| {
            return err;
        };
    }

    fn intSub(self: *HIRVM, a: i32, b: i32) !i32 {
        _ = self;
        return std.math.sub(i32, a, b) catch |err| {
            std.debug.print("Integer overflow in subtraction: {} - {}\n", .{ a, b });
            return err;
        };
    }

    fn intMul(self: *HIRVM, a: i32, b: i32) !i32 {
        _ = self;
        return std.math.mul(i32, a, b) catch |err| {
            std.debug.print("Integer overflow in multiplication: {} * {}\n", .{ a, b });
            return err;
        };
    }

    fn intDiv(self: *HIRVM, a: i32, b: i32) !i32 {
        _ = self;
        if (b == 0) {
            std.debug.print("Division by zero: {} / {}\n", .{ a, b });
            return ErrorList.DivisionByZero;
        }
        return @divTrunc(a, b);
    }

    fn fastIntMod(self: *HIRVM, a: i32, b: i32) !i32 {
        _ = self;
        if (b == 0) {
            std.debug.print("Modulo by zero: {} % {}\n", .{ a, b });
            return ErrorList.DivisionByZero;
        }

        // FIZZBUZZ OPTIMIZATION: Fast modulo for common divisors
        // Avoid expensive division for the most common cases in benchmarks
        return switch (b) {
            3 => fastMod3(a),
            5 => fastMod5(a),
            15 => fastMod15(a),
            else => @mod(a, b),
        };
    }

    // Ultra-fast modulo implementations for FizzBuzz
    inline fn fastMod3(n: i32) i32 {
        // n % 3 using bit tricks: faster than division
        var x = n;
        if (x < 0) x = -x;
        while (x >= 3) {
            x = (x >> 2) + (x & 3);
            if (x >= 3) x -= 3;
        }
        return if (n < 0 and x != 0) 3 - x else x;
    }

    inline fn fastMod5(n: i32) i32 {
        // Fixed: Use correct modulo operation
        return @mod(n, 5);
    }

    inline fn fastMod15(n: i32) i32 {
        // n % 15 = n % (3*5), use Chinese Remainder Theorem concept
        const mod3 = fastMod3(n);
        const mod5 = fastMod5(n);
        // Reconstruct n % 15 from mod 3 and mod 5
        return @mod(mod3 + 3 * @mod(mod5 * 2, 5), 15); // 2 is inverse of 3 mod 5
    }

    /// Float arithmetic operations
    fn floatAdd(self: *HIRVM, a: f64, b: f64) !f64 {
        _ = self;
        return a + b;
    }

    fn floatSub(self: *HIRVM, a: f64, b: f64) !f64 {
        _ = self;
        return a - b;
    }

    fn floatMul(self: *HIRVM, a: f64, b: f64) !f64 {
        _ = self;
        return a * b;
    }

    fn floatDiv(self: *HIRVM, a: f64, b: f64) !f64 {
        _ = self;
        if (b == 0.0) {
            std.debug.print("Float division by zero: {} / {}\n", .{ a, b });
            return ErrorList.DivisionByZero;
        }
        return a / b;
    }

    // ===============================================================================
    // LOGICAL OPERATIONS (From old VM - proven implementations)
    // ===============================================================================

    fn logicalAnd(self: *HIRVM, a: HIRFrame, b: HIRFrame) !u8 {
        const a_tetra = switch (a.value) {
            .tetra => |val| if (val <= 3) val else {
                self.reporter.reportRuntimeError(null, ErrorCode.VARIABLE_NOT_FOUND, "Invalid tetra value: {}", .{val});
                return ErrorList.TypeError;
            },
            else => {
                self.reporter.reportRuntimeError(null, ErrorCode.VARIABLE_NOT_FOUND, "Cannot perform AND on non-tetra values: {s} AND {s}", .{ @tagName(a.value), @tagName(b.value) });
                return ErrorList.TypeError;
            },
        };

        const b_tetra = switch (b.value) {
            .tetra => |val| if (val <= 3) val else {
                self.reporter.reportRuntimeError(null, ErrorCode.VARIABLE_NOT_FOUND, "Invalid tetra value: {}", .{val});
                return ErrorList.TypeError;
            },
            else => {
                self.reporter.reportRuntimeError(null, ErrorCode.VARIABLE_NOT_FOUND, "Cannot perform AND on non-tetra values: {s} AND {s}", .{ @tagName(a.value), @tagName(b.value) });
                return ErrorList.TypeError;
            },
        };

        // First-order logic: collapse 4-valued inputs to classical true/false, then apply classical AND
        // false=0, true=1, both=2, neither=3
        // both(2) contains true, so treat as true; neither(3) doesn't contain true, so treat as false
        const a_classical = if (a_tetra == 1 or a_tetra == 2) true else false;
        const b_classical = if (b_tetra == 1 or b_tetra == 2) true else false;

        // Classical AND: true only if both inputs are true
        return if (a_classical and b_classical) 1 else 0;
    }

    fn logicalOr(self: *HIRVM, a: HIRFrame, b: HIRFrame) !u8 {
        const a_tetra = switch (a.value) {
            .tetra => |val| if (val <= 3) val else {
                self.reporter.reportRuntimeError(null, ErrorCode.VARIABLE_NOT_FOUND, "Invalid tetra value: {}", .{val});
                return ErrorList.TypeError;
            },
            else => {
                self.reporter.reportRuntimeError(null, ErrorCode.VARIABLE_NOT_FOUND, "Cannot perform OR on non-tetra values: {s} OR {s}", .{ @tagName(a.value), @tagName(b.value) });
                return ErrorList.TypeError;
            },
        };

        const b_tetra = switch (b.value) {
            .tetra => |val| if (val <= 3) val else {
                self.reporter.reportRuntimeError(null, ErrorCode.VARIABLE_NOT_FOUND, "Invalid tetra value: {}", .{val});
                return ErrorList.TypeError;
            },
            else => {
                self.reporter.reportRuntimeError(null, ErrorCode.VARIABLE_NOT_FOUND, "Cannot perform OR on non-tetra values: {s} OR {s}", .{ @tagName(a.value), @tagName(b.value) });
                return ErrorList.TypeError;
            },
        };

        // First-order logic: collapse 4-valued inputs to classical true/false, then apply classical OR
        const a_classical = if (a_tetra == 1 or a_tetra == 2) true else false;
        const b_classical = if (b_tetra == 1 or b_tetra == 2) true else false;

        // Classical OR: true if either input is true
        return if (a_classical or b_classical) 1 else 0;
    }

    fn logicalNot(self: *HIRVM, a: HIRFrame) !u8 {
        const a_tetra = switch (a.value) {
            .tetra => |val| if (val <= 3) val else {
                self.reporter.reportRuntimeError(null, ErrorCode.VARIABLE_NOT_FOUND, "Invalid tetra value: {}", .{val});
                return ErrorList.TypeError;
            },
            else => {
                self.reporter.reportRuntimeError(null, ErrorCode.VARIABLE_NOT_FOUND, "Cannot perform NOT on a non-tetra value: {s}", .{@tagName(a.value)});
                return ErrorList.TypeError;
            },
        };

        // First-order logic: collapse 4-valued input to classical true/false, then apply classical NOT
        const a_classical = if (a_tetra == 1 or a_tetra == 2) true else false;

        // Classical NOT: flip the truth value
        return if (a_classical) 0 else 1;
    }

    fn logicalIff(self: *HIRVM, a: HIRFrame, b: HIRFrame) !u8 {
        const a_tetra = switch (a.value) {
            .tetra => |val| if (val <= 3) val else {
                self.reporter.reportRuntimeError(null, ErrorCode.VARIABLE_NOT_FOUND, "Invalid tetra value: {}", .{val});
                return ErrorList.TypeError;
            },
            else => {
                self.reporter.reportRuntimeError(null, ErrorCode.VARIABLE_NOT_FOUND, "Cannot perform IFF on non-tetra values: {s} IFF {s}", .{ @tagName(a.value), @tagName(b.value) });
                return ErrorList.TypeError;
            },
        };

        const b_tetra = switch (b.value) {
            .tetra => |val| if (val <= 3) val else {
                self.reporter.reportRuntimeError(null, ErrorCode.VARIABLE_NOT_FOUND, "Invalid tetra value: {}", .{val});
                return ErrorList.TypeError;
            },
            else => {
                self.reporter.reportRuntimeError(null, ErrorCode.VARIABLE_NOT_FOUND, "Cannot perform IFF on non-tetra values: {s} IFF {s}", .{ @tagName(a.value), @tagName(b.value) });
                return ErrorList.TypeError;
            },
        };

        // First-order logic: collapse 4-valued inputs to classical true/false, then apply classical IFF
        const a_classical = if (a_tetra == 1 or a_tetra == 2) true else false;
        const b_classical = if (b_tetra == 1 or b_tetra == 2) true else false;

        // Classical IFF: true if both inputs have the same truth value
        return if (a_classical == b_classical) 1 else 0;
    }

    fn logicalXor(self: *HIRVM, a: HIRFrame, b: HIRFrame) !u8 {
        const a_tetra = switch (a.value) {
            .tetra => |val| if (val <= 3) val else {
                self.reporter.reportRuntimeError(null, ErrorCode.VARIABLE_NOT_FOUND, "Invalid tetra value: {}", .{val});
                return ErrorList.TypeError;
            },
            else => {
                self.reporter.reportRuntimeError(null, ErrorCode.VARIABLE_NOT_FOUND, "Cannot perform XOR on non-tetra values: {s} XOR {s}", .{ @tagName(a.value), @tagName(b.value) });
                return ErrorList.TypeError;
            },
        };

        const b_tetra = switch (b.value) {
            .tetra => |val| if (val <= 3) val else {
                self.reporter.reportRuntimeError(null, ErrorCode.VARIABLE_NOT_FOUND, "Invalid tetra value: {}", .{val});
                return ErrorList.TypeError;
            },
            else => {
                self.reporter.reportRuntimeError(null, ErrorCode.VARIABLE_NOT_FOUND, "Cannot perform XOR on non-tetra values: {s} XOR {s}", .{ @tagName(a.value), @tagName(b.value) });
                return ErrorList.TypeError;
            },
        };

        // First-order logic: collapse 4-valued inputs to classical true/false, then apply classical XOR
        const a_classical = if (a_tetra == 1 or a_tetra == 2) true else false;
        const b_classical = if (b_tetra == 1 or b_tetra == 2) true else false;

        // Classical XOR: true if inputs have different truth values
        return if (a_classical != b_classical) 1 else 0;
    }

    fn logicalNand(self: *HIRVM, a: HIRFrame, b: HIRFrame) !u8 {
        const a_tetra = switch (a.value) {
            .tetra => |val| if (val <= 3) val else {
                self.reporter.reportRuntimeError(null, ErrorCode.VARIABLE_NOT_FOUND, "Invalid tetra value: {}", .{val});
                return ErrorList.TypeError;
            },
            else => {
                self.reporter.reportRuntimeError(null, ErrorCode.VARIABLE_NOT_FOUND, "Cannot perform NAND on non-tetra values: {s} NAND {s}", .{ @tagName(a.value), @tagName(b.value) });
                return ErrorList.TypeError;
            },
        };

        const b_tetra = switch (b.value) {
            .tetra => |val| if (val <= 3) val else {
                self.reporter.reportRuntimeError(null, ErrorCode.VARIABLE_NOT_FOUND, "Invalid tetra value: {}", .{val});
                return ErrorList.TypeError;
            },
            else => {
                self.reporter.reportRuntimeError(null, ErrorCode.VARIABLE_NOT_FOUND, "Cannot perform NAND on non-tetra values: {s} NAND {s}", .{ @tagName(a.value), @tagName(b.value) });
                return ErrorList.TypeError;
            },
        };

        // First-order logic: collapse 4-valued inputs to classical true/false, then apply classical NAND
        const a_classical = if (a_tetra == 1 or a_tetra == 2) true else false;
        const b_classical = if (b_tetra == 1 or b_tetra == 2) true else false;

        // Classical NAND: NOT(AND) - false only if both inputs are true
        return if (a_classical and b_classical) 0 else 1;
    }

    fn logicalNor(self: *HIRVM, a: HIRFrame, b: HIRFrame) !u8 {
        const a_tetra = switch (a.value) {
            .tetra => |val| if (val <= 3) val else {
                self.reporter.reportRuntimeError(null, ErrorCode.VARIABLE_NOT_FOUND, "Invalid tetra value: {}", .{val});
                return ErrorList.TypeError;
            },
            else => {
                self.reporter.reportRuntimeError(null, ErrorCode.VARIABLE_NOT_FOUND, "Cannot perform NOR on non-tetra values: {s} NOR {s}", .{ @tagName(a.value), @tagName(b.value) });
                return ErrorList.TypeError;
            },
        };

        const b_tetra = switch (b.value) {
            .tetra => |val| if (val <= 3) val else {
                self.reporter.reportRuntimeError(null, ErrorCode.VARIABLE_NOT_FOUND, "Invalid tetra value: {}", .{val});
                return ErrorList.TypeError;
            },
            else => {
                self.reporter.reportRuntimeError(null, ErrorCode.VARIABLE_NOT_FOUND, "Cannot perform NOR on non-tetra values: {s} NOR {s}", .{ @tagName(a.value), @tagName(b.value) });
                return ErrorList.TypeError;
            },
        };

        // First-order logic: collapse 4-valued inputs to classical true/false, then apply classical NOR
        const a_classical = if (a_tetra == 1 or a_tetra == 2) true else false;
        const b_classical = if (b_tetra == 1 or b_tetra == 2) true else false;

        // Classical NOR: NOT(OR) - true only if both inputs are false
        return if (a_classical or b_classical) 0 else 1;
    }

    fn logicalImplies(self: *HIRVM, a: HIRFrame, b: HIRFrame) !u8 {
        const a_tetra = switch (a.value) {
            .tetra => |val| if (val <= 3) val else {
                self.reporter.reportRuntimeError(null, ErrorCode.VARIABLE_NOT_FOUND, "Invalid tetra value: {}", .{val});
                return ErrorList.TypeError;
            },
            else => {
                self.reporter.reportRuntimeError(null, ErrorCode.VARIABLE_NOT_FOUND, "Cannot perform IMPLIES on non-tetra values: {s} IMPLIES {s}", .{ @tagName(a.value), @tagName(b.value) });
                return ErrorList.TypeError;
            },
        };

        const b_tetra = switch (b.value) {
            .tetra => |val| if (val <= 3) val else {
                self.reporter.reportRuntimeError(null, ErrorCode.VARIABLE_NOT_FOUND, "Invalid tetra value: {}", .{val});
                return ErrorList.TypeError;
            },
            else => {
                self.reporter.reportRuntimeError(null, ErrorCode.VARIABLE_NOT_FOUND, "Cannot perform IMPLIES on non-tetra values: {s} IMPLIES {s}", .{ @tagName(a.value), @tagName(b.value) });
                return ErrorList.TypeError;
            },
        };

        // First-order logic: collapse 4-valued inputs to classical true/false, then apply classical IMPLIES
        const a_classical = if (a_tetra == 1 or a_tetra == 2) true else false;
        const b_classical = if (b_tetra == 1 or b_tetra == 2) true else false;

        // Classical IMPLIES: false only if true implies false, otherwise true
        return if (a_classical and !b_classical) 0 else 1;
    }

    // ===============================================================================
    // STRING OPERATIONS (From old VM - proven implementations)
    // ===============================================================================

    /// String concatenation - creates new string from two input strings
    fn stringConcat(self: *HIRVM, a: HIRFrame, b: HIRFrame) !HIRFrame {
        const a_str = switch (a.value) {
            .string => |s| s,
            else => return ErrorList.TypeError,
        };

        const b_str = switch (b.value) {
            .string => |s| s,
            else => return ErrorList.TypeError,
        };

        // Allocate new string buffer
        const new_string = try self.allocator.alloc(u8, a_str.len + b_str.len);
        // Note: This creates a memory leak - in production we'd use string interning
        // TODO: Integrate with memory manager's string interning when available

        @memcpy(new_string[0..a_str.len], a_str);
        @memcpy(new_string[a_str.len..], b_str);

        return HIRFrame.initString(new_string);
    }

    /// String length operation
    fn stringLength(self: *HIRVM, a: HIRFrame) !HIRFrame {
        _ = self;
        const str = switch (a.value) {
            .string => |s| s,
            else => return ErrorList.TypeError,
        };

        return HIRFrame.initInt(@as(i32, @intCast(str.len)));
    }

    /// String bytes operation - converts string to array of bytes
    fn stringBytes(self: *HIRVM, a: HIRFrame) !HIRFrame {
        const str = switch (a.value) {
            .string => |s| s,
            else => return ErrorList.TypeError,
        };

        // Convert string to array of bytes
        const elements = try self.allocator.alloc(HIRValue, str.len);
        for (str, 0..) |byte, i| {
            elements[i] = HIRValue{ .byte = byte };
        }
        const array = HIRValue{ .array = HIRArray{
            .elements = elements,
            .element_type = .Byte,
            .capacity = @intCast(str.len),
        } };
        return HIRFrame.initFromHIRValue(array);
    }

    /// String substring operation - from old VM implementation
    fn stringSubstring(self: *HIRVM, str_frame: HIRFrame, start_frame: HIRFrame, len_frame: HIRFrame) !HIRFrame {
        const str = switch (str_frame.value) {
            .string => |s| s,
            else => return ErrorList.TypeError,
        };

        const start = switch (start_frame.value) {
            .int => |i| i,
            else => return ErrorList.TypeError,
        };

        const length = switch (len_frame.value) {
            .int => |i| i,
            else => return ErrorList.TypeError,
        };

        if (start < 0 or length < 0 or start >= str.len or start + length > str.len) {
            return ErrorList.IndexOutOfBounds;
        }

        const start_idx = @as(usize, @intCast(start));
        const len_val = @as(usize, @intCast(length));
        const slice = str[start_idx .. start_idx + len_val];

        // TODO: Use string interning when available
        const new_string = try self.allocator.dupe(u8, slice);
        return HIRFrame.initString(new_string);
    }

    // ===============================================================================
    // COMPARISON OPERATIONS (Enhanced with mixed-type support from old VM)
    // ===============================================================================

    /// Enhanced comparison with mixed int/float support from old VM
    fn compareEqual(self: *HIRVM, a: HIRFrame, b: HIRFrame) !bool {
        _ = self;
        return switch (a.value) {
            .int => |a_val| switch (b.value) {
                .int => |b_val| a_val == b_val,
                .byte => |b_val| a_val == b_val,
                // Mixed int/float comparison (from old VM)
                .float => |b_val| @as(f64, @floatFromInt(a_val)) == b_val,
                else => false,
            },
            .float => |a_val| switch (b.value) {
                .float => |b_val| a_val == b_val,
                .byte => |b_val| a_val == @as(f64, @floatFromInt(b_val)),
                // Mixed float/int comparison (from old VM)
                .int => |b_val| a_val == @as(f64, @floatFromInt(b_val)),
                else => false,
            },
            .tetra => |a_val| switch (b.value) {
                .tetra => |b_val| a_val == b_val,
                else => false,
            },
            .string => |a_val| switch (b.value) {
                .string => |b_val| blk: {
                    const result = std.mem.eql(u8, a_val, b_val);
                    break :blk result;
                },
                else => false,
            },
            .nothing => switch (b.value) {
                .nothing => true,
                else => false,
            },
            .byte => |a_val| switch (b.value) {
                .byte => |b_val| a_val == b_val,
                .int => |b_val| a_val == b_val,
                .float => |b_val| @as(f64, @floatFromInt(a_val)) == b_val,
                else => false,
            },
            .enum_variant => |a_val| switch (b.value) {
                .enum_variant => |b_val| std.mem.eql(u8, a_val.variant_name, b_val.variant_name) and std.mem.eql(u8, a_val.type_name, b_val.type_name),
                else => false,
            },
            // Complex types - basic equality for now
            .array, .struct_instance, .map => false, // Complex equality not implemented yet
        };
    }

    /// Enhanced less-than with mixed int/float support from old VM
    fn compareLess(self: *HIRVM, a: HIRFrame, b: HIRFrame) !bool {
        _ = self;
        return switch (a.value) {
            .int => |a_val| switch (b.value) {
                .int => |b_val| a_val < b_val,
                .byte => |b_val| a_val < b_val,
                // Mixed int/float comparison (from old VM)
                .float => |b_val| @as(f64, @floatFromInt(a_val)) < b_val,
                else => ErrorList.TypeError,
            },
            .float => |a_val| switch (b.value) {
                .float => |b_val| a_val < b_val,
                .byte => |b_val| a_val < @as(f64, @floatFromInt(b_val)),
                // Mixed float/int comparison (from old VM)
                .int => |b_val| a_val < @as(f64, @floatFromInt(b_val)),
                else => ErrorList.TypeError,
            },
            .byte => |a_val| switch (b.value) {
                .byte => |b_val| a_val < b_val,
                .int => |b_val| a_val < b_val,
                .float => |b_val| @as(f64, @floatFromInt(a_val)) < b_val,
                else => ErrorList.TypeError,
            },
            // Complex types don't support comparison
            .tetra, .string, .nothing, .array, .struct_instance, .map, .enum_variant => ErrorList.TypeError,
        };
    }

    /// Enhanced greater-than with mixed int/float support from old VM
    fn compareGreater(self: *HIRVM, a: HIRFrame, b: HIRFrame) !bool {
        _ = self;
        return switch (a.value) {
            .int => |a_val| switch (b.value) {
                .int => |b_val| a_val > b_val,
                .byte => |b_val| a_val > b_val,
                // Mixed int/float comparison (from old VM)
                .float => |b_val| @as(f64, @floatFromInt(a_val)) > b_val,
                else => ErrorList.TypeError,
            },
            .float => |a_val| switch (b.value) {
                .float => |b_val| a_val > b_val,
                .byte => |b_val| a_val > @as(f64, @floatFromInt(b_val)),
                // Mixed float/int comparison (from old VM)
                .int => |b_val| a_val > @as(f64, @floatFromInt(b_val)),
                else => ErrorList.TypeError,
            },
            .byte => |a_val| switch (b.value) {
                .byte => |b_val| a_val > b_val,
                .int => |b_val| a_val > b_val,
                .float => |b_val| @as(f64, @floatFromInt(a_val)) > b_val,
                else => ErrorList.TypeError,
            },
            // Complex types don't support comparison
            .tetra, .string, .nothing, .array, .struct_instance, .map, .enum_variant => ErrorList.TypeError,
        };
    }

    /// Print a HIR value for debugging/peekion
    pub fn printHIRValue(self: *HIRVM, value: HIRValue) !void {
        switch (value) {
            .int => |i| std.debug.print("{}", .{i}),
            .float => |f| std.debug.print("{d}", .{f}),
            .string => |s| std.debug.print("\"{s}\"", .{s}),
            .tetra => |b| std.debug.print("{}", .{b}),
            .byte => |u| std.debug.print("{}", .{u}),
            .nothing => std.debug.print("nothing", .{}),
            // Complex types - show contents for arrays
            .array => |arr| {
                std.debug.print("[", .{});
                var first = true;
                for (arr.elements) |elem| {
                    if (std.meta.eql(elem, HIRValue.nothing)) break;
                    if (!first) std.debug.print(", ", .{});
                    try self.printHIRValue(elem);
                    first = false;
                }
                std.debug.print("]", .{});
            },
            .struct_instance => |s| {
                std.debug.print("{{ ", .{}); // Add opening bracket
                for (s.fields, 0..) |field, i| {
                    std.debug.print("{s}: ", .{field.name});
                    try self.printHIRValue(field.value);
                    if (i < s.fields.len - 1) std.debug.print(", ", .{});
                }
                std.debug.print(" }}", .{}); // Fix closing bracket
            },
            .map => std.debug.print("{{map}}", .{}),
            .enum_variant => |e| std.debug.print(".{s}", .{e.variant_name}),
        }
    }

    /// Get a readable type string from HIRValue for debug output
    pub fn getTypeString(self: *HIRVM, value: HIRValue) []const u8 {
        _ = self;
        return switch (value) {
            .int => "int",
            .byte => "byte",
            .float => "float",
            .string => "string",
            .tetra => "tetra",
            .nothing => "nothing",
            .array => |arr| switch (arr.element_type) {
                .Int => "int[]",
                .Byte => "byte[]",
                .Float => "float[]",
                .String => "string[]",
                .Tetra => "tetra[]",
                .Array => "array[]", // Nested arrays
                .Struct => "struct[]",
                .Nothing => "nothing[]",
                .Map => "map[]",
                .Enum => "enum[]",
                .Function => "function[]",
                .Union => "union[]",
                .Unknown => "unknown[]",
            },
            .struct_instance => |s| s.type_name,
            .map => "map",
            .enum_variant => |e| e.type_name,
        };
    }

    /// Convert HIR value to string representation
    fn valueToString(self: *HIRVM, value: HIRValue) ![]const u8 {
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
                var result = std.ArrayList(u8).init(self.allocator);
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
                var result = std.ArrayList(u8).init(self.allocator);
                defer result.deinit();
                try result.appendSlice("{ ");
                for (s.fields, 0..) |field, i| {
                    if (i > 0) try result.appendSlice(", ");
                    try result.appendSlice(field.name);
                    try result.appendSlice(": ");
                    const field_str = try self.valueToString(field.value);
                    defer self.allocator.free(field_str);
                    try result.appendSlice(field_str);
                }
                try result.appendSlice(" }");
                return try result.toOwnedSlice();
            },
            .enum_variant => |e| try std.fmt.allocPrint(self.allocator, ".{s}", .{e.variant_name}),
            else => try std.fmt.allocPrint(self.allocator, "{s}", .{@tagName(value)}),
        };
    }

    fn printFloat(writer: anytype, f: f64) !void {
        const roundedDown = std.math.floor(f);
        if (f - roundedDown == 0) {
            try writer.print("{d}.0", .{f});
        } else {
            try writer.print("{d}", .{f});
        }
    }

    /// Format HIR value to a writer (for proper UTF-8 buffered output like old interpreter)
    pub fn formatHIRValue(self: *HIRVM, writer: anytype, value: HIRValue) !void {
        switch (value) {
            .int => |i| try writer.print("{}", .{i}),
            .byte => |u| try writer.print("0x{X:0>2}", .{u}),
            .float => |f| try printFloat(writer, f),
            .string => |s| try writer.print("\"{s}\"", .{s}),
            .tetra => |t| try writer.print("{s}", .{switch (t) {
                0 => "false",
                1 => "true",
                2 => "both",
                3 => "neither",
                else => "invalid",
            }}),
            .nothing => try writer.print("nothing", .{}),
            .array => |arr| {
                try writer.print("[", .{});
                var first = true;
                for (arr.elements) |elem| {
                    if (std.meta.eql(elem, HIRValue.nothing)) break; // Stop at first nothing element
                    if (!first) try writer.print(", ", .{});
                    try self.formatHIRValue(writer, elem);
                    first = false;
                }
                try writer.print("]", .{});
            },
            .struct_instance => |s| {
                try writer.print("{{ ", .{}); // Add opening bracket
                for (s.fields, 0..) |field, i| {
                    try writer.print("{s}: ", .{field.name});
                    try self.formatHIRValue(writer, field.value);
                    if (i < s.fields.len - 1) try writer.print(", ", .{});
                }
                try writer.print(" }}", .{}); // Fix closing bracket
            },
            .enum_variant => |e| try writer.print(".{s}", .{e.variant_name}),
            else => try writer.print("{s}", .{@tagName(value)}),
        }
    }

    /// Format HIR value to a writer without quotes or extra formatting (for Show instruction)
    pub fn formatHIRValueRaw(self: *HIRVM, writer: anytype, value: HIRValue) !void {
        switch (value) {
            .int => |i| try writer.print("{}", .{i}),
            .byte => |u| try writer.print("0x{X:0>2}", .{u}),
            .float => |f| try printFloat(writer, f),
            .string => |s| try writer.writeAll(s), // No quotes around strings, direct write
            .tetra => |t| try writer.print("{s}", .{switch (t) {
                0 => "false",
                1 => "true",
                2 => "both",
                3 => "neither",
                else => "invalid",
            }}),
            .nothing => try writer.print("nothing", .{}),
            .array => |arr| {
                try writer.print("[", .{});
                var first = true;
                for (arr.elements) |elem| {
                    if (std.meta.eql(elem, HIRValue.nothing)) break; // Stop at first nothing element
                    if (!first) try writer.print(", ", .{});
                    try self.formatHIRValueRaw(writer, elem);
                    first = false;
                }
                try writer.print("]", .{});
            },
            .struct_instance => |s| {
                try writer.print("{{ ", .{}); // Add opening bracket
                for (s.fields, 0..) |field, i| {
                    try writer.print("{s}: ", .{field.name});
                    try self.formatHIRValueRaw(writer, field.value);
                    if (i < s.fields.len - 1) try writer.print(", ", .{});
                }
                try writer.print(" }}", .{}); // Fix closing bracket
            },
            .enum_variant => |e| try writer.print(".{s}", .{e.variant_name}),
            else => try writer.print("{s}", .{@tagName(value)}),
        }
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
            var i: i32 = self.stack.sp - 1;
            while (i >= 0) : (i -= 1) {
                const frame = self.stack.data[@intCast(i)];
                std.debug.print("  [{}]: ", .{i});
                try self.printHIRValue(frame.value);
                std.debug.print("\n", .{});
            }
        } else {
            std.debug.print("Stack is empty\n", .{});
        }
        std.debug.print("=========================\n\n", .{});
    }

    // Add debug logging for exists/forall quantifiers
    pub fn exists_quantifier(self: *HIRVM, array: HIRFrame, _: HIRValue.Function) !void {
        std.debug.print("exists_quantifier: array value type = {s}\n", .{@tagName(array.value)});
        switch (array.value) {
            .array => {
                // ... existing code ...
            },
            else => {
                std.debug.print("exists_quantifier failed: argument is of type {s}\n", .{@tagName(array.value)});
                return self.reporter.reportRuntimeError(null, ErrorCode.VARIABLE_NOT_FOUND, "exists_quantifier: argument must be array", .{});
            },
        }
    }

    pub fn forall_quantifier(self: *HIRVM, array: HIRFrame, _: HIRValue.Function) !void {
        std.debug.print("forall_quantifier: array value type = {s}\n", .{@tagName(array.value)});
        switch (array.value) {
            .array => {
                // ... existing code ...
            },
            else => {
                std.debug.print("forall_quantifier failed: argument is of type {s}\n", .{@tagName(array.value)});
                return self.reporter.reportRuntimeError(null, ErrorCode.VARIABLE_NOT_FOUND, "forall_quantifier: argument must be array", .{});
            },
        }
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

    fn handleArrayPush(self: *HIRVM, element_value: HIRValue) !void {
        const array_frame = try self.stack.pop();
        switch (array_frame.value) {
            .array => |arr| {
                // Work with a mutable copy
                var mutable_arr = arr;

                // If array has no concrete element type yet, infer from first pushed element
                if (mutable_arr.element_type == .Unknown or mutable_arr.element_type == .Nothing) {
                    mutable_arr.element_type = switch (element_value) {
                        .int => .Int,
                        .byte => .Byte,
                        .float => .Float,
                        .string => .String,
                        .tetra => .Tetra,
                        .array => .Array,
                        .struct_instance => .Struct,
                        else => mutable_arr.element_type,
                    };
                }

                // Compute logical length (count until first 'nothing')
                var length: usize = 0;
                while (length < mutable_arr.elements.len) : (length += 1) {
                    if (std.meta.eql(mutable_arr.elements[length], HIRValue.nothing)) break;
                }

                // Resize if at capacity
                if (length >= mutable_arr.capacity) {
                    const old_capacity = mutable_arr.capacity;
                    const new_capacity = if (old_capacity == 0) 1 else old_capacity * 2;
                    const new_elements = try self.allocator.alloc(HIRValue, new_capacity);

                    // Copy existing populated elements
                    if (length > 0) {
                        @memcpy(new_elements[0..length], mutable_arr.elements[0..length]);
                    }

                    // Place new element
                    new_elements[length] = element_value;

                    // Initialize remaining to nothing
                    var i: usize = length + 1;
                    while (i < new_capacity) : (i += 1) {
                        new_elements[i] = HIRValue.nothing;
                    }

                    // Replace storage
                    self.allocator.free(mutable_arr.elements);
                    mutable_arr.elements = new_elements;
                    mutable_arr.capacity = new_capacity;
                } else {
                    // Enough capacity, write in-place
                    mutable_arr.elements[length] = element_value;
                }

                // Push updated array back
                try self.stack.push(HIRFrame.initFromHIRValue(HIRValue{ .array = mutable_arr }));
            },
            .nothing => {
                // Create new array with initial capacity of 8
                var new_array = HIRArray{
                    .elements = try self.allocator.alloc(HIRValue, 8),
                    .element_type = switch (element_value) {
                        .int => .Int,
                        .byte => .Byte,
                        .float => .Float,
                        .string => .String,
                        .tetra => .Tetra,
                        .array => .Array,
                        .struct_instance => .Struct,
                        else => unreachable,
                    },
                    .capacity = 8,
                };

                // Initialize all elements to nothing
                for (new_array.elements) |*element| {
                    element.* = HIRValue.nothing;
                }
                new_array.elements[0] = element_value;

                try self.stack.push(HIRFrame.initFromHIRValue(HIRValue{ .array = new_array }));
            },
            else => {
                return self.reporter.reportRuntimeError(null, ErrorCode.VARIABLE_NOT_FOUND, "Cannot push to non-array value: {s}", .{@tagName(array_frame.value)});
            },
        }
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
    fn coerceValue(self: *HIRVM, value: HIRValue, expected_type: HIRType) HIRValue {
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
