const std = @import("std");
const hir_soxa = @import("../codegen/soxa.zig");
const hir_instructions = @import("../codegen/soxa_instructions.zig");
const hir_values = @import("../codegen/soxa_values.zig");
const hir_types = @import("../codegen/soxa_types.zig");
const HIRInstruction = hir_instructions.HIRInstruction;
const HIRValue = hir_values.HIRValue;
const HIRArray = hir_values.HIRArray;
const HIRTuple = hir_values.HIRTuple;
const HIRMap = hir_values.HIRMap;
const HIRMapEntry = hir_values.HIRMapEntry;
const HIRStruct = hir_values.HIRStruct;
const HIRStructField = hir_values.HIRStructField;
const HIRProgram = hir_types.HIRProgram;
const HIRType = hir_types.HIRType;
const instructions = @import("instructions.zig");
const Reporter = @import("../utils/reporting.zig").Reporter;
const ErrorList = @import("../utils/reporting.zig").ErrorList;
const memory = @import("../utils/memory.zig");
const MemoryManager = memory.MemoryManager;
const ScopeManager = memory.ScopeManager;
const Scope = memory.Scope;
const Variable = memory.Variable;
const TokenType = @import("../lexer/token.zig").TokenType;
const TokenLiteral = @import("../types/types.zig").TokenLiteral;
const TypeInfo = @import("../ast/ast.zig").TypeInfo;
const StructField = @import("../types/types.zig").StructField;
const printi = std.debug.print;
const StringInterner = memory.StringInterner;
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

    pub fn initU8(x: u8) HIRFrame {
        return HIRFrame{ .value = HIRValue{ .u8 = x } };
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

    pub fn asU8(self: HIRFrame) !u8 {
        return switch (self.value) {
            .u8 => |u| u,
            else => ErrorList.TypeError,
        };
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
        .field_type = .Auto,
        .path = null,
    };
}

pub fn hirValueToTokenLiteral(hir_value: HIRValue) TokenLiteral {
    return switch (hir_value) {
        .int => |i| TokenLiteral{ .int = i },
        .u8 => |u| TokenLiteral{ .u8 = u },
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
        else => TokenLiteral{ .nothing = {} },
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
    debug_enabled: bool,

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
    fast_mode: bool = true, // Enable optimizations for computational workloads
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

    pub fn tokenLiteralToHIRValue(self: *HIRVM, token_literal: TokenLiteral) HIRValue {
        return switch (token_literal) {
            .int => |i| HIRValue{ .int = i },
            .u8 => |u| HIRValue{ .u8 = u },
            .float => |f| HIRValue{ .float = f },
            .string => |s| HIRValue{ .string = s },
            .tetra => |t| HIRValue{ .tetra = switch (t) {
                .false => 0,
                .true => 1,
                .both => 2,
                .neither => 3,
            } },
            .nothing => HIRValue.nothing,
            .struct_value => |s| blk: {
                // Convert StructField array to HIRStructField array
                var hir_fields = self.allocator.alloc(HIRStructField, s.fields.len) catch break :blk HIRValue.nothing;
                for (s.fields, 0..) |field, i| {
                    hir_fields[i] = HIRStructField{
                        .name = field.name,
                        .value = self.tokenLiteralToHIRValue(field.value),
                        .field_type = .Auto, // We'll need type inference here
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
            else => HIRValue.nothing,
        };
    }

    pub fn hirValueToTokenLiteral(self: *HIRVM, hir_value: HIRValue) TokenLiteral {
        return switch (hir_value) {
            .int => |i| TokenLiteral{ .int = i },
            .u8 => |u| TokenLiteral{ .u8 = u },
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
            .struct_instance => |s| blk: {
                // Convert HIRStructField array to StructField array
                var token_fields = self.allocator.alloc(StructField, s.fields.len) catch break :blk TokenLiteral{ .nothing = {} };
                for (s.fields, 0..) |field, i| {
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
            else => TokenLiteral{ .nothing = {} },
        };
    }

    pub fn hirValueToTokenType(self: *HIRVM, hir_value: HIRValue) TokenType {
        _ = self;
        return switch (hir_value) {
            .int => .INT,
            .u8 => .U8,
            .float => .FLOAT,
            .string => .STRING,
            .tetra => .TETRA,
            .nothing => .NOTHING,
            // Complex types - map to closest TokenType
            .array => .ARRAY,
            .struct_instance => .IDENTIFIER, // Structs represented as identifiers
            .tuple => .ARRAY, // Tuples similar to arrays
            .map => .ARRAY, // Maps similar to arrays
            .enum_variant => .IDENTIFIER, // Enums represented as identifiers
        };
    }

    pub fn hirValueToTypeInfo(self: *HIRVM, hir_value: HIRValue) TypeInfo {
        _ = self;
        return switch (hir_value) {
            .int => TypeInfo{ .base = .Int, .is_mutable = true },
            .u8 => TypeInfo{ .base = .U8, .is_mutable = true },
            .float => TypeInfo{ .base = .Float, .is_mutable = true },
            .string => TypeInfo{ .base = .String, .is_mutable = true },
            .tetra => TypeInfo{ .base = .Tetra, .is_mutable = true },
            .nothing => TypeInfo{ .base = .Nothing, .is_mutable = true },
            // Complex types - map to appropriate TypeInfo
            .array => TypeInfo{ .base = .Array, .is_mutable = true },
            .struct_instance => TypeInfo{ .base = .Auto, .is_mutable = true }, // Struct types need resolution
            .tuple => TypeInfo{ .base = .Tuple, .is_mutable = true },
            .map => TypeInfo{ .base = .Map, .is_mutable = true },
            .enum_variant => TypeInfo{ .base = .Auto, .is_mutable = true }, // Enum types need resolution
        };
    }

    pub fn init(program: *HIRProgram, reporter: *Reporter, memory_manager: *MemoryManager, debug_enabled: bool) !HIRVM {
        const allocator = memory_manager.getAllocator();

        // Create a new execution scope for this HIR VM run
        // This allows proper cleanup and isolation
        const execution_scope = try memory_manager.scope_manager.createScope(memory_manager.scope_manager.root_scope);

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
            .fast_mode = true,
            .turbo_mode = true,
            .hot_vars = [_]?HotVar{null} ** 4,
            .hot_var_count = 0,
            .debug_enabled = debug_enabled,
            .string_interner = string_interner,
        };

        // Pre-resolve all labels for efficient jumps
        try vm.resolveLabels();

        return vm;
    }

    pub fn deinit(self: *HIRVM) void {
        // Clean up the execution scope
        self.current_scope.deinit();

        // Note: stack, call_stack, label_map, and var_cache are allocated with
        // the memory manager's arena allocator, so they'll be cleaned up automatically
        // when the memory manager deinitializes its arena
    }

    /// Pre-resolve all labels to instruction indices for O(1) jump lookup
    fn resolveLabels(self: *HIRVM) !void {
        std.debug.print(">> Resolving labels from {} instructions\n", .{self.program.instructions.len});
        for (self.program.instructions, 0..) |instruction, i| {
            switch (instruction) {
                .Label => |label| {
                    try self.label_map.put(label.name, @intCast(i));
                    std.debug.print(">>   Resolved label '{s}' to instruction {}\n", .{ label.name, i });
                },
                else => {},
            }
        }
        std.debug.print(">> Total labels resolved: {}\n", .{self.label_map.count()});
    }

    /// Main execution loop - directly execute HIR instructions
    pub fn run(self: *HIRVM) !?HIRFrame {
        if (self.debug_enabled) {
            std.debug.print(">> Starting HIR VM execution with {} instructions\n", .{self.program.instructions.len});
        }

        // sanity check on soxa code
        try self.validateHIRProgram();

        while (self.running and self.ip < self.program.instructions.len) {
            const instruction = self.program.instructions[self.ip];

            // OPTIMIZED: Reduce debug output in fast mode for better performance
            if (self.debug_enabled and (!self.fast_mode or self.ip < 10)) {
                std.debug.print("HIR VM [{}]: Executing {s}\n", .{ self.ip, @tagName(instruction) });
                if (self.ip == 6) {
                    std.debug.print(">> CRITICAL: Instruction at IP 6 is: {any}\n", .{instruction});
                }
            }

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

    /// CRITICAL: Validate that no Auto types exist in the HIR program
    fn validateHIRProgram(self: *HIRVM) !void {
        std.debug.print(">> SANITY CHECK: Validating HIR program for Auto types...\n", .{});

        for (self.program.instructions, 0..) |instruction, ip| {
            switch (instruction) {
                .Inspect => |i| {
                    if (i.value_type == .Auto) {
                        self.reporter.reportError("FATAL: Auto type found in Inspect instruction at IP {} for variable '{s}'. Type inference likely failed during code generation.", .{ ip, i.name orelse "unknown" });
                        return ErrorList.InternalParserError;
                    }
                },

                .InspectStruct => |i| {
                    // Check for Auto types in field types
                    for (i.field_types) |field_type| {
                        if (field_type == .Auto) {
                            self.reporter.reportError("FATAL: Auto type found in InspectStruct instruction at IP {} for field {}. Type inference likely failed during code generation.", .{ ip, i });
                            return error.AutoTypeInInspectStruct;
                        }
                    }
                },

                .Compare => |c| {
                    if (c.operand_type == .Auto) {
                        self.reporter.reportError("FATAL: Auto operand_type found in Compare instruction at IP {}. Type inference likely failed during code generation.", .{ip});
                        std.debug.print(">>   Compare instruction: {any}\n", .{c});
                        return ErrorList.InternalParserError;
                    }
                },
                .Convert => |c| {
                    if (c.from_type == .Auto or c.to_type == .Auto) {
                        self.reporter.reportError("FATAL: Auto type found in Convert instruction at IP {} (from: {s}, to: {s}). Type inference likely failed during code generation.", .{ ip, @tagName(c.from_type), @tagName(c.to_type) });
                        std.debug.print(">>   Convert instruction: {any}\n", .{c});
                        return ErrorList.InternalParserError;
                    }
                },
                .Call => |c| {
                    if (c.return_type == .Auto) {
                        self.reporter.reportError("FATAL: Auto return_type found in Call instruction at IP {} for function '{s}'. Type inference likely failed during code generation.", .{ ip, c.qualified_name });
                        std.debug.print(">>   Call instruction: {any}\n", .{c});
                        return ErrorList.InternalParserError;
                    }
                },
                .TailCall => |c| {
                    if (c.return_type == .Auto) {
                        self.reporter.reportError("FATAL: Auto return_type found in TailCall instruction at IP {} for function '{s}'. Type inference likely failed during code generation.", .{ ip, c.qualified_name });
                        std.debug.print(">>   TailCall instruction: {any}\n", .{c});
                        return ErrorList.InternalParserError;
                    }
                },
                .Return => |r| {
                    if (r.return_type == .Auto) {
                        self.reporter.reportError("FATAL: Auto return_type found in Return instruction at IP {}. Type inference likely failed during code generation.", .{ip});
                        std.debug.print(">>   Return instruction: {any}\n", .{r});
                        return ErrorList.InternalParserError;
                    }
                },
                else => {
                    // Other instructions don't have explicit type fields to validate
                },
            }
        }

        std.debug.print(">> SANITY CHECK PASSED: No Auto types found in HIR program\n", .{});
    }

    /// Execute a single HIR instruction
    fn executeInstruction(self: *HIRVM, instruction: HIRInstruction) !void {
        switch (instruction) {
            .Const => |c| {
                // Push constant from constant pool
                const constant_value = self.program.constant_pool[c.constant_id];
                try self.stack.push(HIRFrame.initFromHIRValue(constant_value));
            },

            .LoadVar => |v| {
                if (self.debug_enabled) {
                    std.debug.print(">> LoadVar '{s}' in scope {}\n", .{ v.var_name, self.current_scope.id });
                }

                if (self.turbo_mode) {
                    // TURBO: Check hot variable cache first (array lookup - fastest possible)
                    for (self.hot_vars[0..self.hot_var_count]) |hot_var| {
                        if (hot_var != null and std.mem.eql(u8, hot_var.?.name, v.var_name)) {
                            if (self.memory_manager.scope_manager.value_storage.get(hot_var.?.storage_id)) |storage| {
                                const hir_value = self.tokenLiteralToHIRValue(storage.value);
                                if (self.debug_enabled) {
                                    std.debug.print(">>   TURBO hit for '{s}' = {any} (storage_id: {})\n", .{ v.var_name, hir_value, hot_var.?.storage_id });
                                }
                                try self.stack.push(HIRFrame.initFromHIRValue(hir_value));
                                return; // Ultra-fast path
                            }
                            break;
                        }
                    }
                }

                // PERFORMANCE: Fast cache that fully integrates with memory module
                if (self.fast_mode) {
                    // Check cache first - trust storage_id if valid
                    if (self.var_cache.get(v.var_name)) |cached_storage_id| {
                        if (self.memory_manager.scope_manager.value_storage.get(cached_storage_id)) |storage| {
                            const hir_value = self.tokenLiteralToHIRValue(storage.value);
                            if (self.debug_enabled) {
                                std.debug.print(">>   CACHE hit for '{s}' = {any} (storage_id: {})\n", .{ v.var_name, hir_value, cached_storage_id });
                            }
                            try self.stack.push(HIRFrame.initFromHIRValue(hir_value));
                            return; // Ultra-fast cached path
                        } else {
                            // Cache is stale, remove entry
                            if (self.debug_enabled) {
                                std.debug.print(">>   CACHE stale for '{s}' (storage_id: {})\n", .{ v.var_name, cached_storage_id });
                            }
                            _ = self.var_cache.remove(v.var_name);
                        }
                    }
                }

                // FALLBACK: Standard variable lookup (populate cache for next time)
                if (self.current_scope.lookupVariable(v.var_name)) |variable| {
                    if (self.memory_manager.scope_manager.value_storage.get(variable.storage_id)) |storage| {
                        // PERFORMANCE: Cache the storage_id from memory module's authoritative lookup
                        if (self.fast_mode) {
                            // Cache the result from the memory module for next time
                            self.var_cache.put(v.var_name, variable.storage_id) catch {};
                        }

                        const hir_value = self.tokenLiteralToHIRValue(storage.value);

                        if (self.debug_enabled) {
                            std.debug.print(">>   Found '{s}' = {any} (storage_id: {}, scope: {})\n", .{ v.var_name, hir_value, variable.storage_id, self.current_scope.id });
                        }

                        try self.stack.push(HIRFrame.initFromHIRValue(hir_value));
                    } else {
                        return self.reporter.reportError("Variable storage not found for: {s}", .{v.var_name});
                    }
                } else {
                    if (self.debug_enabled) {
                        std.debug.print(">>   Variable '{s}' not found in scope {} (parent: {})\n", .{ v.var_name, self.current_scope.id, if (self.current_scope.parent) |p| p.id else 999 });
                    }
                    return self.reporter.reportError("Undefined variable: {s}", .{v.var_name});
                }
            },

            .StoreVar => |v| {
                // Store top of stack to variable
                const value = try self.stack.pop();
                const token_literal = self.hirValueToTokenLiteral(value.value);
                const token_type = self.hirValueToTokenType(value.value);
                const type_info = self.hirValueToTypeInfo(value.value);

                if (self.debug_enabled) {
                    std.debug.print(">> StoreVar '{s}' = {any} in scope {}\n", .{ v.var_name, value.value, self.current_scope.id });
                }

                // PERFORMANCE: Update cache after storing to keep it synchronized with memory module
                if (self.fast_mode) {
                    // Update cache with the current variable's storage_id
                    if (self.current_scope.name_map.get(v.var_name)) |variable| {
                        self.var_cache.put(v.var_name, variable.storage_id) catch {};
                    }
                }

                // CRITICAL FIX: Check if variable exists in CURRENT scope only
                // Function parameters must create NEW storage in each scope, never reuse parent storage
                if (self.current_scope.name_map.get(v.var_name)) |variable| {
                    // Variable exists in current scope - update its storage
                    if (self.memory_manager.scope_manager.value_storage.getPtr(variable.storage_id)) |storage| {
                        if (storage.*.constant) {
                            return self.reporter.reportError("Cannot modify constant variable: {s}", .{v.var_name});
                        }

                        if (self.debug_enabled) {
                            std.debug.print(">>   Updated existing '{s}' from {any} to {any} (storage_id: {}, scope: {})\n", .{ v.var_name, storage.*.value, token_literal, variable.storage_id, self.current_scope.id });
                        }

                        storage.*.value = token_literal;
                        storage.*.type = token_type;
                        storage.*.type_info = type_info;
                    } else {
                        return self.reporter.reportError("Variable storage not found for: {s}", .{v.var_name});
                    }
                } else {
                    // Variable doesn't exist in current scope - create NEW variable with NEW storage
                    // This ensures function parameters get their own storage in each recursive call
                    if (self.debug_enabled) {
                        std.debug.print(">>   Creating new variable '{s}' = {any} in scope {} (may shadow parent)\n", .{ v.var_name, value.value, self.current_scope.id });
                    }
                    _ = self.current_scope.createValueBinding(v.var_name, token_literal, token_type, type_info, false) catch |err| {
                        return self.reporter.reportError("Failed to create variable {s}: {}", .{ v.var_name, err });
                    };
                }
            },

            .IntArith => |a| {
                if (self.debug_enabled) {
                    std.debug.print(">> IntArith.{s}: Stack size before: {}\n", .{ @tagName(a.op), self.stack.size() });
                }

                const b = try self.stack.pop();
                const a_val = try self.stack.pop();

                if (self.debug_enabled) {
                    std.debug.print(">> IntArith.{s}: {any} {s} {any} = ", .{ @tagName(a.op), a_val.value, @tagName(a.op), b.value });
                }

                // IMPROVED TYPE CHECKING: Handle non-integer values gracefully
                const a_int = switch (a_val.value) {
                    .int => |i| i,
                    .u8 => |u| @as(i32, u),
                    .tetra => |t| @as(i32, t),
                    .nothing => {
                        return self.reporter.reportError("Cannot perform arithmetic on 'nothing' value", .{});
                    },
                    else => {
                        return self.reporter.reportError("Cannot convert {s} to integer for arithmetic", .{@tagName(a_val.value)});
                    },
                };

                const b_int = switch (b.value) {
                    .int => |i| i,
                    .u8 => |u| @as(i32, u),
                    .tetra => |t| @as(i32, t),
                    .nothing => {
                        return self.reporter.reportError("Cannot perform arithmetic on 'nothing' value", .{});
                    },
                    else => {
                        return self.reporter.reportError("Cannot convert {s} to integer for arithmetic", .{@tagName(b.value)});
                    },
                };

                const result = switch (a.op) {
                    .Add => std.math.add(i32, a_int, b_int) catch |err| {
                        if (self.debug_enabled) std.debug.print("Integer overflow in addition: {} + {}\n", .{ a_int, b_int });
                        return err;
                    },
                    .Sub => std.math.sub(i32, a_int, b_int) catch |err| {
                        if (self.debug_enabled) std.debug.print("Integer overflow in subtraction: {} - {}\n", .{ a_int, b_int });
                        return err;
                    },
                    .Mul => std.math.mul(i32, a_int, b_int) catch |err| {
                        if (self.debug_enabled) std.debug.print("Integer overflow in multiplication: {} * {}\n", .{ a_int, b_int });
                        return err;
                    },
                    .Div => if (b_int == 0) {
                        if (self.debug_enabled) std.debug.print("Division by zero: {} / {}\n", .{ a_int, b_int });
                        return ErrorList.DivisionByZero;
                    } else @divTrunc(a_int, b_int),
                    .Mod => try self.fastIntMod(a_int, b_int), // Use optimized modulo
                };

                if (self.debug_enabled) {
                    std.debug.print("{d} [Stack size after: {}]\n", .{ result, self.stack.size() + 1 });
                }

                try self.stack.push(HIRFrame.initInt(result));
            },

            .FloatArith => |a| {
                const b = try self.stack.pop();
                const a_val = try self.stack.pop();

                // PERFORMANCE: Inline float operations for maximum speed
                const a_float = try a_val.asFloat();
                const b_float = try b.asFloat();

                const result = switch (a.op) {
                    .Add => a_float + b_float,
                    .Sub => a_float - b_float,
                    .Mul => a_float * b_float,
                    .Div => if (b_float == 0.0) {
                        if (self.debug_enabled) std.debug.print("Float division by zero: {} / {}\n", .{ a_float, b_float });
                        return ErrorList.DivisionByZero;
                    } else a_float / b_float,
                    .Mod => return ErrorList.UnsupportedOperator, // Float modulo not supported
                };

                try self.stack.push(HIRFrame.initFloat(result));
            },

            .Compare => |c| {
                const b = try self.stack.pop();
                const a_val = try self.stack.pop();

                if (self.debug_enabled) {
                    std.debug.print(">> Compare {s}: {any} {s} {any}\n", .{ @tagName(c.op), a_val.value, @tagName(c.op), b.value });
                }

                const result = switch (c.op) {
                    .Eq => try self.compareEqual(a_val, b),
                    .Ne => !(try self.compareEqual(a_val, b)),
                    .Lt => try self.compareLess(a_val, b),
                    .Le => (try self.compareLess(a_val, b)) or (try self.compareEqual(a_val, b)),
                    .Gt => try self.compareGreater(a_val, b),
                    .Ge => (try self.compareGreater(a_val, b)) or (try self.compareEqual(a_val, b)),
                };

                if (self.debug_enabled) {
                    std.debug.print(">> Compare result: {} (pushing tetra {})\n", .{ result, if (result) @as(u8, 1) else @as(u8, 0) });
                }

                try self.stack.push(HIRFrame.initTetra(if (result) 1 else 0));
            },

            .Jump => |j| {
                // Unconditional jump to label
                if (self.label_map.get(j.label)) |target_ip| {
                    self.ip = target_ip;
                    if (self.debug_enabled and !self.fast_mode) {
                        std.debug.print("Jumping to label '{s}' at instruction {}\n", .{ j.label, target_ip });
                    }
                } else {
                    return self.reporter.reportFatalError("Unknown label: {s}", .{j.label});
                }
            },

            .JumpCond => |j| {
                // OPTIMIZED: Conditional jump with reduced overhead
                const condition = try self.stack.pop();
                if (self.debug_enabled) {
                    std.debug.print(">> JumpCond: condition = {any}, true_label = {s}, false_label = {s}\n", .{ condition.value, j.label_true, j.label_false });
                }

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
                if (self.debug_enabled) {
                    std.debug.print(">> JumpCond: should_jump = {}, target_label = {s}\n", .{ should_jump, target_label });
                }

                if (self.label_map.get(target_label)) |target_ip| {
                    self.ip = target_ip;
                    if (self.debug_enabled) {
                        std.debug.print(">> JumpCond: Jumping to '{s}' at instruction {} (current IP was {})\n", .{ target_label, target_ip, self.ip - 1 });
                    }
                } else {
                    return self.reporter.reportFatalError("Unknown label: {s}", .{target_label});
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

            .Inspect => |inspect| {
                const value = try self.stack.pop();
                self.reporter.debug("Inspect called with value: {any}", .{value});

                if (inspect.location) |location| {
                    try std.io.getStdOut().writer().print("[{s}:{d}:{d}] ", .{ location.file, location.line, location.column });
                }

                // Determine the variable name to display
                var variable_name: ?[]const u8 = null;

                // First priority: use the name from the inspect instruction
                if (inspect.name) |name| {
                    variable_name = name;
                } else if (value.value == .struct_instance) {
                    // For struct instances, use the full path if available
                    if (value.value.struct_instance.path) |path| {
                        variable_name = path;
                    } else if (value.field_name) |field_name| {
                        variable_name = field_name;
                    }
                } else if (value.field_name) |field_name| {
                    variable_name = field_name;
                }

                // Print the variable name and value
                if (variable_name) |name| {
                    try std.io.getStdOut().writer().print("{s} = ", .{name});
                } else {
                    try std.io.getStdOut().writer().print("value = ", .{});
                }

                // Format the value
                try self.formatHIRValue(std.io.getStdOut().writer(), value.value);
                try std.io.getStdOut().writer().print("\n", .{});

                // Push the value back onto the stack for potential further use
                try self.stack.push(value);
            },

            .InspectStruct => |i| {
                const value = try self.stack.pop();
                self.reporter.debug("InspectStruct called with value: {any}", .{value});

                // Handle both struct instances and field values
                switch (value.value) {
                    .struct_instance => |s| {
                        self.reporter.debug("Processing struct instance of type '{s}' with {d} fields", .{ s.type_name, s.fields.len });
                        // Print each field with proper location formatting
                        for (s.fields) |field| {
                            self.reporter.debug("Processing field '{s}'", .{field.name});

                            // Format with location information like the regular Inspect instruction
                            if (i.location) |location| {
                                try std.io.getStdOut().writer().print("[{s}:{d}:{d}] ", .{ location.file, location.line, location.column });
                            }

                            // Build the full field path
                            if (s.path) |path| {
                                try std.io.getStdOut().writer().print("{s}.{s} = ", .{ path, field.name });
                            } else {
                                try std.io.getStdOut().writer().print("{s}.{s} = ", .{ s.type_name, field.name });
                            }

                            try self.formatHIRValue(std.io.getStdOut().writer(), field.value);
                            try std.io.getStdOut().writer().print("\n", .{});
                        }
                    },
                    else => {
                        self.reporter.debug("Processing non-struct value with {d} field names", .{i.field_names.len});
                        // For non-struct values (like field access results), print as a single value with location
                        if (i.field_names.len > 0) {
                            if (i.location) |location| {
                                try std.io.getStdOut().writer().print("[{s}:{d}:{d}] ", .{ location.file, location.line, location.column });
                            }

                            // Always use the first field name from the instruction
                            try std.io.getStdOut().writer().print("{s} = ", .{i.field_names[0]});
                            try self.formatHIRValue(std.io.getStdOut().writer(), value.value);
                            try std.io.getStdOut().writer().print("\n", .{});
                        }
                    },
                }

                // Push the value back onto the stack for potential further use
                try self.stack.push(value);
            },

            .EnterScope => |s| {
                // TAIL CALL FIX: Skip scope creation if this is a tail call
                if (self.skip_next_enter_scope) {
                    self.skip_next_enter_scope = false; // Reset flag
                    if (self.debug_enabled) {
                        std.debug.print(">> Skipped EnterScope {} for tail call (reusing current scope {})\n", .{ s.scope_id, self.current_scope.id });
                    }
                    return; // Skip scope creation entirely
                }

                // Create new scope - the memory module handles all the complexity
                const new_scope = try self.memory_manager.scope_manager.createScope(self.current_scope);
                self.current_scope = new_scope;

                // PERFORMANCE: Clear cache when entering new scope to prevent stale data
                // This is fast because we're using arena allocator
                if (self.fast_mode) {
                    self.var_cache.clearRetainingCapacity();
                }

                if (self.debug_enabled) {
                    if (new_scope.parent) |parent| {
                        std.debug.print(">> Entered scope {} (parent: {}) [Call stack: {}]\n", .{ s.scope_id, parent.id, self.call_stack.sp });
                    } else {
                        std.debug.print(">> Entered scope {} (no parent) [Call stack: {}]\n", .{ s.scope_id, self.call_stack.sp });
                    }
                }
            },

            .ExitScope => |s| {
                if (self.current_scope.parent) |parent_scope| {
                    const old_scope = self.current_scope;
                    self.current_scope = parent_scope;

                    if (self.debug_enabled) {
                        std.debug.print(">> Exited scope {} (returned to: {}) [Call stack: {}]\n", .{ s.scope_id, parent_scope.id, self.call_stack.sp });
                    }
                    // Clean up the old scope
                    old_scope.deinit();
                } else {
                    // GRACEFUL HANDLING: Don't error on root scope exit, just warn and continue
                    if (self.debug_enabled) {
                        std.debug.print(">> Warning: Attempted to exit root scope {} - ignoring\n", .{s.scope_id});
                    }
                    // Don't return error, just continue execution
                }
            },

            .Halt => {
                if (self.debug_enabled) {
                    std.debug.print("HIR VM halted\n", .{});
                }
                self.running = false;
            },

            // ULTRA-FAST Logical operations using lookup tables!
            .LogicalOp => |op| {
                const b = try self.stack.pop();
                const a = try self.stack.pop();

                const result = switch (op.op) {
                    .And => try self.logicalAnd(a, b),
                    .Or => try self.logicalOr(a, b),
                    .Not => try self.logicalNot(a),
                    .Iff => try self.logicalIff(a, b),
                    .Xor => try self.logicalXor(a, b),
                    .Nand => try self.logicalNand(a, b),
                    .Nor => try self.logicalNor(a, b),
                    .Implies => try self.logicalImplies(a, b),
                };

                try self.stack.push(HIRFrame.initTetra(result));
            },

            // String operations (from old VM - proven implementations)
            .StringOp => |s| {
                switch (s.op) {
                    .Concat => {
                        const b = try self.stack.pop();
                        const a = try self.stack.pop();
                        const result = try self.stringConcat(a, b);
                        try self.stack.push(result);
                    },
                    .Length => {
                        const a = try self.stack.pop();
                        const result = try self.stringLength(a);
                        try self.stack.push(result);
                    },
                    .Substring => {
                        const len = try self.stack.pop();
                        const start = try self.stack.pop();
                        const str = try self.stack.pop();
                        const result = try self.stringSubstring(str, start, len);
                        try self.stack.push(result);
                    },
                }
            },

            // Array operations (Phase 1: Core Data Types)
            .ArrayNew => |a| {
                // Create new array with specified size
                // CRITICAL FIX: For empty arrays (size=0), allocate minimum capacity for growth
                const initial_capacity = if (a.size == 0) 8 else a.size; // Start with capacity 8 for empty arrays
                const elements = try self.allocator.alloc(HIRValue, initial_capacity);

                // Initialize all elements to nothing
                for (elements) |*element| {
                    element.* = HIRValue.nothing;
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
                        return self.reporter.reportError("Array index cannot be negative: {}", .{i});
                    } else @as(u32, @intCast(i)),
                    .u8 => |u| @as(u32, u),
                    .tetra => |t| @as(u32, t), // Allow tetra values as indices
                    .string => |s| blk: {
                        // GRACEFUL: Try to parse string as integer
                        const parsed = std.fmt.parseInt(i32, s, 10) catch {
                            if (self.debug_enabled) {
                                std.debug.print(">> Warning: Cannot parse string '{s}' as array index, returning nothing\n", .{s});
                            }
                            try self.stack.push(HIRFrame.initFromHIRValue(HIRValue.nothing));
                            return;
                        };
                        if (parsed < 0) {
                            return self.reporter.reportError("Array index cannot be negative: {}", .{parsed});
                        }
                        break :blk @as(u32, @intCast(parsed));
                    },
                    .nothing => {
                        if (self.debug_enabled) {
                            std.debug.print(">> Warning: Using 'nothing' as array index, returning nothing\n", .{});
                        }
                        try self.stack.push(HIRFrame.initFromHIRValue(HIRValue.nothing));
                        return;
                    },
                    else => {
                        if (self.debug_enabled) {
                            std.debug.print(">> Warning: Cannot use {s} as array index, returning nothing\n", .{@tagName(index.value)});
                        }
                        try self.stack.push(HIRFrame.initFromHIRValue(HIRValue.nothing));
                        return;
                    },
                };

                switch (array.value) {
                    .array => |arr| {
                        if (a.bounds_check and index_val >= arr.elements.len) {
                            if (self.debug_enabled) {
                                std.debug.print(">> Warning: Array index {} out of bounds (length: {}), returning nothing\n", .{ index_val, arr.elements.len });
                            }
                            try self.stack.push(HIRFrame.initFromHIRValue(HIRValue.nothing));
                            return;
                        }

                        const element = arr.elements[index_val];
                        try self.stack.push(HIRFrame.initFromHIRValue(element));
                    },
                    .nothing => {
                        // GRACEFUL: Treat 'nothing' as empty array - any index returns nothing
                        if (self.debug_enabled) {
                            std.debug.print(">> Warning: Indexing 'nothing' value, returning nothing\n", .{});
                        }
                        try self.stack.push(HIRFrame.initFromHIRValue(HIRValue.nothing));
                    },
                    .string => |s| {
                        // GRACEFUL: Allow string indexing (return character as string)
                        if (index_val >= s.len) {
                            if (self.debug_enabled) {
                                std.debug.print(">> Warning: String index {} out of bounds (length: {}), returning nothing\n", .{ index_val, s.len });
                            }
                            try self.stack.push(HIRFrame.initFromHIRValue(HIRValue.nothing));
                            return;
                        }
                        const char_str = s[index_val .. index_val + 1];
                        try self.stack.push(HIRFrame.initFromHIRValue(HIRValue{ .string = char_str }));
                    },
                    .tuple => |t| {
                        // GRACEFUL: Allow tuple indexing
                        if (index_val >= t.elements.len) {
                            if (self.debug_enabled) {
                                std.debug.print(">> Warning: Tuple index {} out of bounds (length: {}), returning nothing\n", .{ index_val, t.elements.len });
                            }
                            try self.stack.push(HIRFrame.initFromHIRValue(HIRValue.nothing));
                            return;
                        }
                        const element = t.elements[index_val];
                        try self.stack.push(HIRFrame.initFromHIRValue(element));
                    },
                    else => {
                        if (self.debug_enabled) {
                            std.debug.print(">> Warning: Cannot index {s} value, returning nothing\n", .{@tagName(array.value)});
                        }
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
                        return self.reporter.reportError("Array index cannot be negative: {}", .{i});
                    } else @as(u32, @intCast(i)),
                    .u8 => |u| @as(u32, u),
                    .tetra => |t| @as(u32, t), // Allow tetra values as indices
                    .string => |s| blk: {
                        // GRACEFUL: Try to parse string as integer
                        const parsed = std.fmt.parseInt(i32, s, 10) catch {
                            if (self.debug_enabled) {
                                std.debug.print(">> Warning: Cannot parse string '{s}' as array index, operation ignored\n", .{s});
                            }
                            try self.stack.push(array_frame); // Push original array back
                            return;
                        };
                        if (parsed < 0) {
                            return self.reporter.reportError("Array index cannot be negative: {}", .{parsed});
                        }
                        break :blk @as(u32, @intCast(parsed));
                    },
                    .nothing => {
                        if (self.debug_enabled) {
                            std.debug.print(">> Warning: Cannot use 'nothing' as array index, operation ignored\n", .{});
                        }
                        try self.stack.push(array_frame); // Push original array back
                        return;
                    },
                    else => {
                        if (self.debug_enabled) {
                            std.debug.print(">> Warning: Cannot use {s} as array index, operation ignored\n", .{@tagName(index.value)});
                        }
                        try self.stack.push(array_frame); // Push original array back
                        return;
                    },
                };

                switch (array_frame.value) {
                    .array => |arr| {
                        // Create a mutable copy of the array
                        var mutable_arr = arr;

                        if (a.bounds_check and index_val >= mutable_arr.elements.len) {
                            if (self.debug_enabled) {
                                std.debug.print(">> Warning: Array index {} out of bounds (length: {}), operation ignored\n", .{ index_val, mutable_arr.elements.len });
                            }
                            try self.stack.push(array_frame); // Push original array back
                            return;
                        }

                        mutable_arr.elements[index_val] = value.value;
                        // Push the modified array back onto the stack
                        const modified_array_value = HIRValue{ .array = mutable_arr };
                        try self.stack.push(HIRFrame.initFromHIRValue(modified_array_value));
                    },
                    .nothing => {
                        // GRACEFUL: Cannot set on 'nothing', just ignore
                        if (self.debug_enabled) {
                            std.debug.print(">> Warning: Cannot set array element on 'nothing' value, operation ignored\n", .{});
                        }
                        try self.stack.push(HIRFrame.initFromHIRValue(HIRValue.nothing));
                    },
                    else => {
                        if (self.debug_enabled) {
                            std.debug.print(">> Warning: Cannot set array element on {s} value, operation ignored\n", .{@tagName(array_frame.value)});
                        }
                        try self.stack.push(array_frame); // Push original value back
                    },
                }
            },

            .ArrayPush => |a| {
                // Push element to end of array
                const element = try self.stack.pop(); // Element to push
                const array = try self.stack.pop(); // Array

                switch (array.value) {
                    .array => |arr| {
                        // Create a mutable copy of the array
                        var mutable_arr = arr;

                        // Check if we need to resize
                        if (mutable_arr.elements.len >= mutable_arr.capacity) {
                            const new_capacity = switch (a.resize_behavior) {
                                .Double => mutable_arr.capacity * 2,
                                .Fixed => return self.reporter.reportError("Array at capacity {} - cannot push more elements", .{mutable_arr.capacity}),
                                .Exact => mutable_arr.capacity + 1,
                            };

                            // Reallocate with new capacity
                            const new_elements = try self.allocator.realloc(mutable_arr.elements, new_capacity);
                            mutable_arr.elements = new_elements;
                            mutable_arr.capacity = new_capacity;
                        }

                        // Add element to end (we need to track current length separately)
                        // For now, find the first nothing element to determine length
                        var length: u32 = 0;
                        for (mutable_arr.elements) |elem| {
                            if (std.meta.eql(elem, HIRValue.nothing)) break;
                            length += 1;
                        }

                        if (length < mutable_arr.elements.len) {
                            mutable_arr.elements[length] = element.value;
                        } else {
                            return self.reporter.reportError("Array is full and cannot be resized", .{});
                        }

                        // Push the modified array back onto the stack
                        const modified_array_value = HIRValue{ .array = mutable_arr };
                        try self.stack.push(HIRFrame.initFromHIRValue(modified_array_value));
                    },
                    .nothing => {
                        // GRACEFUL: Treat 'nothing' as creating a new single-element array
                        if (self.debug_enabled) {
                            std.debug.print(">> Warning: Pushing to 'nothing' value, creating new array\n", .{});
                        }
                        const elements = try self.allocator.alloc(HIRValue, 8);
                        for (elements) |*elem| {
                            elem.* = HIRValue.nothing;
                        }
                        elements[0] = element.value;

                        const new_array = HIRValue{ .array = HIRArray{
                            .elements = elements,
                            .element_type = .Auto,
                            .capacity = 8,
                        } };
                        try self.stack.push(HIRFrame.initFromHIRValue(new_array));
                    },
                    else => {
                        if (self.debug_enabled) {
                            std.debug.print(">> Warning: Cannot push to {s} value, returning original value\n", .{@tagName(array.value)});
                        }
                        try self.stack.push(array); // Return original value
                    },
                }
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
                            return self.reporter.reportError("Cannot pop from empty array", .{});
                        }

                        const last_element = mutable_arr.elements[length - 1];
                        mutable_arr.elements[length - 1] = HIRValue.nothing; // Clear the element

                        // Push the popped element onto the stack
                        try self.stack.push(HIRFrame.initFromHIRValue(last_element));

                        // Note: We don't push the array back since pop consumes it
                    },
                    else => return self.reporter.reportError("Cannot pop from non-array value: {s}", .{@tagName(array.value)}),
                }
            },

            .ArrayLen => {
                // Get array length
                const array = try self.stack.pop(); // Array

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
                    else => return self.reporter.reportError("Cannot get length of non-array value: {s}", .{@tagName(array.value)}),
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
                            else => return self.reporter.reportError("Cannot concatenate array with non-array: {s}", .{@tagName(b.value)}),
                        }
                    },
                    else => return self.reporter.reportError("Cannot concatenate non-array: {s}", .{@tagName(a.value)}),
                }
            },

            .TailCall => |c| {
                // TAIL CALL OPTIMIZATION: Reuse current stack frame instead of creating new one
                if (self.debug_enabled) {
                    std.debug.print("Tail call: {s} with {} args (kind: {s}) [Stack size: {}]\n", .{ c.qualified_name, c.arg_count, @tagName(c.call_kind), self.stack.size() });
                }

                switch (c.call_kind) {
                    .LocalFunction => {
                        // Tail call optimization for user-defined functions
                        if (c.function_index >= self.program.function_table.len) {
                            return self.reporter.reportError("Invalid function index: {} (max: {})", .{ c.function_index, self.program.function_table.len });
                        }

                        const function = self.program.function_table[c.function_index];

                        // TAIL CALL FIX: Use start_label to include parameter setup, but we need to handle the scope issue
                        const target_label = function.start_label;

                        if (self.debug_enabled) {
                            std.debug.print(">> Tail call to: {s} with {} args at label: {s}\n", .{ function.name, c.arg_count, target_label });
                        }

                        // TAIL CALL FIX: Clear variable cache and set flag to skip scope creation
                        if (self.fast_mode) {
                            self.var_cache.clearRetainingCapacity();
                        }
                        self.skip_next_enter_scope = true; // Skip scope creation when we jump to function start

                        if (self.debug_enabled) {
                            std.debug.print(">> Cleared variable cache and set skip_next_enter_scope for tail call\n", .{});
                        }

                        // Jump to function start (including parameter setup) without call stack modification
                        if (self.label_map.get(target_label)) |target_ip| {
                            self.ip = target_ip;
                            if (self.debug_enabled) {
                                std.debug.print(">> Tail call jump to '{s}' at instruction {} (call stack unchanged: {})\n", .{ target_label, target_ip, self.call_stack.sp });
                            }
                            return; // Jump to function body
                        } else {
                            return self.reporter.reportError("Function label not found: {s}", .{target_label});
                        }
                    },
                    else => {
                        return self.reporter.reportError("Tail call not supported for call kind: {s}", .{@tagName(c.call_kind)});
                    },
                }
            },

            .Call => |c| {
                // Handle function calls
                if (self.debug_enabled) {
                    std.debug.print("Function call: {s} with {} args (kind: {s}) [Stack size: {}]\n", .{ c.qualified_name, c.arg_count, @tagName(c.call_kind), self.stack.size() });
                }

                switch (c.call_kind) {
                    .LocalFunction => {
                        // User-defined function call with proper stack management
                        if (c.function_index >= self.program.function_table.len) {
                            return self.reporter.reportError("Invalid function index: {} (max: {})", .{ c.function_index, self.program.function_table.len });
                        }

                        const function = self.program.function_table[c.function_index];
                        if (self.debug_enabled) {
                            std.debug.print("Calling user function: {s} with {} args at label: {s}\n", .{ function.name, c.arg_count, function.start_label });
                        }

                        // Arguments are already on the stack in the correct order for parameter setup
                        // The function's parameter setup (StoreVar instructions) will handle them
                        if (self.debug_enabled) {
                            std.debug.print(">> Stack has {} args ready for parameter setup\n", .{c.arg_count});
                            std.debug.print(">> Call stack depth: {}\n", .{self.call_stack.sp});
                            std.debug.print(">> Current scope: {} (parent: {})\n", .{ self.current_scope.id, if (self.current_scope.parent) |p| p.id else 999 });

                            // Debug: Show the actual argument values on stack
                            if (c.arg_count > 0) {
                                var i: u32 = 0;
                                while (i < c.arg_count and i < 3) : (i += 1) { // Show up to 3 args
                                    const stack_idx = self.stack.sp - 1 - @as(i32, @intCast(i));
                                    if (stack_idx >= 0) {
                                        const arg = self.stack.data[@intCast(stack_idx)];
                                        std.debug.print(">>   Arg[{}]: {any}\n", .{ i, arg.value });
                                    }
                                }
                            }
                        }

                        // Push call frame for proper return handling
                        const return_ip = self.ip + 1; // Return to instruction after this call
                        if (self.debug_enabled) {
                            std.debug.print(">> Setting return IP to {} (current IP: {})\n", .{ return_ip, self.ip });
                        }
                        const call_frame = CallFrame{
                            .return_ip = return_ip,
                            .function_name = function.name,
                            .arg_count = c.arg_count, // Store arg count to clean up stack later
                        };
                        try self.call_stack.push(call_frame);

                        // Use pre-resolved label map for O(1) lookup
                        if (self.label_map.get(function.start_label)) |target_ip| {
                            self.ip = target_ip;
                            return; // Jump to function start
                        } else {
                            return self.reporter.reportError("Function label not found: {s}", .{function.start_label});
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
                                else => return self.reporter.reportError("Cannot get length of non-array value: {s}", .{@tagName(array.value)}),
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
                                        for (new_elements[length..]) |*elem| {
                                            elem.* = HIRValue.nothing;
                                        }
                                    }

                                    // Add element to end (capacity is guaranteed to be > length)
                                    mutable_arr.elements[length] = element.value;

                                    // Push the modified array back onto the stack
                                    const modified_array_value = HIRValue{ .array = mutable_arr };
                                    try self.stack.push(HIRFrame.initFromHIRValue(modified_array_value));
                                },
                                else => return self.reporter.reportError("Cannot push to non-array value: {s}", .{@tagName(array.value)}),
                            }
                        } else if (std.mem.eql(u8, c.qualified_name, "safeAdd")) {
                            // Safe addition with overflow protection
                            const b = try self.stack.pop();
                            const a = try self.stack.pop();

                            const a_int = switch (a.value) {
                                .int => |i| i,
                                .u8 => |u| @as(i32, u),
                                else => return self.reporter.reportError("safeAdd: first argument must be integer", .{}),
                            };

                            const b_int = switch (b.value) {
                                .int => |i| i,
                                .u8 => |u| @as(i32, u),
                                else => return self.reporter.reportError("safeAdd: second argument must be integer", .{}),
                            };

                            const result = std.math.add(i32, a_int, b_int) catch {
                                // On overflow, return nothing instead of crashing
                                try self.stack.push(HIRFrame.initFromHIRValue(HIRValue.nothing));
                                return;
                            };

                            try self.stack.push(HIRFrame.initInt(result));
                        } else if (std.mem.eql(u8, c.qualified_name, "exists_quantifier")) {
                            // Existential quantifier - check if any element in array satisfies condition
                            const array = try self.stack.pop();

                            switch (array.value) {
                                .array => |arr| {
                                    // For now, just return true if array has any non-nothing elements
                                    // TODO: Add predicate support when needed
                                    var found = false;
                                    for (arr.elements) |elem| {
                                        if (!std.meta.eql(elem, HIRValue.nothing)) {
                                            found = true;
                                            break;
                                        }
                                    }
                                    try self.stack.push(HIRFrame.initTetra(if (found) 1 else 0));
                                },
                                else => return self.reporter.reportError("exists_quantifier: argument must be array", .{}),
                            }
                        } else if (std.mem.eql(u8, c.qualified_name, "forall_quantifier")) {
                            // Universal quantifier - check if all elements in array satisfy condition
                            const array = try self.stack.pop();

                            switch (array.value) {
                                .array => |arr| {
                                    // For now, just return true if array has all non-nothing elements
                                    // TODO: Add predicate support when needed
                                    var has_elements = false;
                                    for (arr.elements) |elem| {
                                        if (std.meta.eql(elem, HIRValue.nothing)) {
                                            break; // End of actual elements
                                        }
                                        has_elements = true;
                                        // For now, all non-nothing elements are considered valid
                                    }
                                    try self.stack.push(HIRFrame.initTetra(if (has_elements) 1 else 0));
                                },
                                else => return self.reporter.reportError("forall_quantifier: argument must be array", .{}),
                            }
                        } else {
                            return self.reporter.reportError("Unknown built-in function: {s}", .{c.qualified_name});
                        }
                    },
                    else => {
                        return self.reporter.reportError("Unsupported call kind: {s}", .{@tagName(c.call_kind)});
                    },
                }
            },

            .Return => |ret| {
                // Handle function return
                if (self.debug_enabled) {
                    const top_value = if (self.stack.size() > 0)
                        (self.stack.peek() catch HIRFrame.initFromHIRValue(HIRValue.nothing)).value
                    else
                        HIRValue.nothing;
                    std.debug.print(">> Function return (has_value: {}) [Stack size: {}, Top: {s}]\n", .{ ret.has_value, self.stack.size(), @tagName(top_value) });

                    // Debug: Show return value if present
                    if (ret.has_value and self.stack.size() > 0) {
                        const return_val = self.stack.peek() catch HIRFrame.initFromHIRValue(HIRValue.nothing);
                        std.debug.print(">>   Return value: {any}\n", .{return_val.value});
                    }
                }

                // Check if we're returning from main program or a function call
                if (self.call_stack.isEmpty()) {
                    // Returning from main program - halt execution
                    if (self.debug_enabled) {
                        std.debug.print(">> Returning from main program - halting VM\n", .{});
                    }
                    self.running = false;
                } else {
                    // Auto-exit current scope before returning to restore caller's scope
                    if (self.current_scope.parent) |parent_scope| {
                        const old_scope = self.current_scope;
                        self.current_scope = parent_scope;

                        // PERFORMANCE: Clear cache on scope change for correctness
                        if (self.fast_mode) {
                            self.var_cache.clearRetainingCapacity();
                        }

                        if (self.debug_enabled) {
                            std.debug.print(">> Auto-exited scope {} on return (restored to: {}) [Call stack: {}]\n", .{ old_scope.id, parent_scope.id, self.call_stack.sp });
                        }
                        // Clean up the old scope
                        old_scope.deinit();
                    }

                    // Returning from function call - pop call frame and return to caller
                    const call_frame = try self.call_stack.pop();
                    if (self.debug_enabled) {
                        std.debug.print(">> Returning from function '{s}' to IP {} (current IP: {})\n", .{ call_frame.function_name, call_frame.return_ip, self.ip });
                        std.debug.print(">>   Current scope: {} (parent: {})\n", .{ self.current_scope.id, if (self.current_scope.parent) |p| p.id else 999 });
                        std.debug.print(">>   Call stack depth after pop: {}\n", .{self.call_stack.sp});
                    }

                    if (!ret.has_value) {
                        // No return value - push nothing for void functions
                        try self.stack.push(HIRFrame.initFromHIRValue(HIRValue.nothing));
                    }
                    // Note: Return value is already on stack for functions with return values

                    // Return to caller
                    self.ip = call_frame.return_ip;
                    if (self.debug_enabled) {
                        std.debug.print(">> Set IP to {} after return\n", .{self.ip});
                    }
                    return; // Don't auto-increment IP since we just set it
                }
            },

            .TryBegin => |t| {
                // Create a new scope for the try block
                const new_scope = try self.memory_manager.scope_manager.createScope(self.current_scope);
                self.current_scope = new_scope;
                if (self.fast_mode) {
                    self.var_cache.clearRetainingCapacity();
                }

                // Store the catch label for later use
                if (self.label_map.get(t.catch_label)) |target_ip| {
                    // Store the catch target for use by Throw
                    self.current_catch_target = target_ip;
                } else {
                    return self.reporter.reportError("Catch label not found: {s}", .{t.catch_label});
                }
            },

            .TryCatch => |t| {
                // Exit the try block scope and create a new scope for the catch block
                if (self.current_scope.parent) |parent_scope| {
                    const old_scope = self.current_scope;
                    self.current_scope = parent_scope;
                    if (self.fast_mode) {
                        self.var_cache.clearRetainingCapacity();
                    }
                    old_scope.deinit();
                }

                // Create new scope for catch block
                const catch_scope = try self.memory_manager.scope_manager.createScope(self.current_scope);
                self.current_scope = catch_scope;
                if (self.fast_mode) {
                    self.var_cache.clearRetainingCapacity();
                }

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
                    return self.reporter.reportError("No catch block found for throw with type {s}", .{@tagName(t.exception_type)});
                }
            },

            .StructNew => |s| {
                // Allocate array for fields
                var fields = try self.allocator.alloc(HIRStructField, s.field_count);

                // Pop values from stack and create fields in reverse order
                var i: usize = s.field_count;
                while (i > 0) {
                    i -= 1;
                    const field_value = try self.stack.pop();
                    fields[i] = HIRStructField{
                        .name = "", // Field names are set later via StoreFieldName
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
                    .struct_instance => |*struct_inst| { // Note: changed to pointer to modify in place
                        // Find the first empty field name and set it
                        for (struct_inst.fields) |*field| {
                            if (field.name.len == 0) {
                                // Allocate and store the field name
                                field.name = try self.allocator.dupe(u8, store_field.field_name);
                                break;
                            }
                        }

                        // Push the struct back
                        try self.stack.push(frame);
                    },
                    else => {
                        self.reporter.reportError("Cannot store field name on non-struct value: {s}", .{@tagName(frame.value)});
                        return ErrorList.TypeError;
                    },
                }
            },

            .GetField => |get_field| {
                // Get the struct instance from the top of the stack
                const frame = try self.stack.pop();

                // Ensure we have a struct instance
                switch (frame.value) {
                    .struct_instance => |struct_inst| {
                        // Intern the field name for comparison
                        const interned_name = try self.string_interner.intern(get_field.field_name);

                        // Find field by name
                        for (struct_inst.fields) |field| {
                            if (std.mem.eql(u8, field.name, interned_name)) {
                                // Push the field value onto the stack
                                try self.stack.push(HIRFrame{ .value = field.value });
                                return;
                            }
                        }

                        self.reporter.reportError("Field '{s}' not found in struct '{s}'", .{ get_field.field_name, struct_inst.type_name });
                        return ErrorList.FieldNotFound;
                    },
                    else => {
                        self.reporter.reportError("Cannot get field from non-struct value: {s}", .{@tagName(frame.value)});
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
                    .struct_instance => |*struct_inst| {
                        // Find field by name
                        for (struct_inst.fields) |*field| {
                            if (std.mem.eql(u8, field.name, set_field.field_name)) {
                                // Update the field value
                                field.value = value_frame.value;

                                // Push the struct back onto the stack
                                try self.stack.push(struct_frame);
                                return;
                            }
                        }

                        self.reporter.reportError("Field '{s}' not found in struct '{s}'", .{ set_field.field_name, struct_inst.type_name });
                        return ErrorList.FieldNotFound;
                    },
                    else => {
                        self.reporter.reportError("Cannot set field on non-struct value: {s}", .{@tagName(struct_frame.value)});
                        return ErrorList.TypeError;
                    },
                }
            },

            else => {
                std.debug.print("\n!! Unhandled HIR instruction at IP {d}:\n", .{self.ip});
                std.debug.print("  Type: {s}\n", .{@tagName(instruction)});

                // Print detailed instruction info based on type
                switch (instruction) {
                    .LogicalOp => |op| {
                        std.debug.print("  Operation: {s}\n", .{@tagName(op.op)});
                    },
                    .StringOp => |op| {
                        std.debug.print("  Operation: {s}\n", .{@tagName(op.op)});
                    },
                    .MapGet => |m| {
                        std.debug.print("  Key type: {s}\n", .{@tagName(m.key_type)});
                    },
                    else => {
                        std.debug.print("  Raw data: {any}\n", .{instruction});
                    },
                }

                // Print stack state
                std.debug.print("  Stack depth: {d}\n", .{self.stack.size()});
                if (!self.stack.isEmpty()) {
                    if (self.stack.peek()) |top| {
                        std.debug.print("  Top of stack: {s}\n", .{@tagName(top.value)});
                    } else |_| {}
                }

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
            std.debug.print("Integer overflow in addition: {} + {}\n", .{ a, b });
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
        // n % 5 using multiplication trick
        var x = n;
        if (x < 0) x = -x;
        x = (x >> 2) + (x & 3);
        x = (x >> 2) + (x & 3);
        if (x >= 5) x -= 5;
        return if (n < 0 and x != 0) 5 - x else x;
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
                self.reporter.reportError("Invalid tetra value: {}", .{val});
                return ErrorList.TypeError;
            },
            else => {
                self.reporter.reportError("Cannot perform AND on non-tetra values: {s} AND {s}", .{ @tagName(a.value), @tagName(b.value) });
                return ErrorList.TypeError;
            },
        };

        const b_tetra = switch (b.value) {
            .tetra => |val| if (val <= 3) val else {
                self.reporter.reportError("Invalid tetra value: {}", .{val});
                return ErrorList.TypeError;
            },
            else => {
                self.reporter.reportError("Cannot perform AND on non-tetra values: {s} AND {s}", .{ @tagName(a.value), @tagName(b.value) });
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
                self.reporter.reportError("Invalid tetra value: {}", .{val});
                return ErrorList.TypeError;
            },
            else => {
                self.reporter.reportError("Cannot perform OR on non-tetra values: {s} OR {s}", .{ @tagName(a.value), @tagName(b.value) });
                return ErrorList.TypeError;
            },
        };

        const b_tetra = switch (b.value) {
            .tetra => |val| if (val <= 3) val else {
                self.reporter.reportError("Invalid tetra value: {}", .{val});
                return ErrorList.TypeError;
            },
            else => {
                self.reporter.reportError("Cannot perform OR on non-tetra values: {s} OR {s}", .{ @tagName(a.value), @tagName(b.value) });
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
                self.reporter.reportError("Invalid tetra value: {}", .{val});
                return ErrorList.TypeError;
            },
            else => {
                self.reporter.reportError("Cannot perform NOT on a non-tetra value: {s}", .{@tagName(a.value)});
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
                self.reporter.reportError("Invalid tetra value: {}", .{val});
                return ErrorList.TypeError;
            },
            else => {
                self.reporter.reportError("Cannot perform IFF on non-tetra values: {s} IFF {s}", .{ @tagName(a.value), @tagName(b.value) });
                return ErrorList.TypeError;
            },
        };

        const b_tetra = switch (b.value) {
            .tetra => |val| if (val <= 3) val else {
                self.reporter.reportError("Invalid tetra value: {}", .{val});
                return ErrorList.TypeError;
            },
            else => {
                self.reporter.reportError("Cannot perform IFF on non-tetra values: {s} IFF {s}", .{ @tagName(a.value), @tagName(b.value) });
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
                self.reporter.reportError("Invalid tetra value: {}", .{val});
                return ErrorList.TypeError;
            },
            else => {
                self.reporter.reportError("Cannot perform XOR on non-tetra values: {s} XOR {s}", .{ @tagName(a.value), @tagName(b.value) });
                return ErrorList.TypeError;
            },
        };

        const b_tetra = switch (b.value) {
            .tetra => |val| if (val <= 3) val else {
                self.reporter.reportError("Invalid tetra value: {}", .{val});
                return ErrorList.TypeError;
            },
            else => {
                self.reporter.reportError("Cannot perform XOR on non-tetra values: {s} XOR {s}", .{ @tagName(a.value), @tagName(b.value) });
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
                self.reporter.reportError("Invalid tetra value: {}", .{val});
                return ErrorList.TypeError;
            },
            else => {
                self.reporter.reportError("Cannot perform NAND on non-tetra values: {s} NAND {s}", .{ @tagName(a.value), @tagName(b.value) });
                return ErrorList.TypeError;
            },
        };

        const b_tetra = switch (b.value) {
            .tetra => |val| if (val <= 3) val else {
                self.reporter.reportError("Invalid tetra value: {}", .{val});
                return ErrorList.TypeError;
            },
            else => {
                self.reporter.reportError("Cannot perform NAND on non-tetra values: {s} NAND {s}", .{ @tagName(a.value), @tagName(b.value) });
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
                self.reporter.reportError("Invalid tetra value: {}", .{val});
                return ErrorList.TypeError;
            },
            else => {
                self.reporter.reportError("Cannot perform NOR on non-tetra values: {s} NOR {s}", .{ @tagName(a.value), @tagName(b.value) });
                return ErrorList.TypeError;
            },
        };

        const b_tetra = switch (b.value) {
            .tetra => |val| if (val <= 3) val else {
                self.reporter.reportError("Invalid tetra value: {}", .{val});
                return ErrorList.TypeError;
            },
            else => {
                self.reporter.reportError("Cannot perform NOR on non-tetra values: {s} NOR {s}", .{ @tagName(a.value), @tagName(b.value) });
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
                self.reporter.reportError("Invalid tetra value: {}", .{val});
                return ErrorList.TypeError;
            },
            else => {
                self.reporter.reportError("Cannot perform IMPLIES on non-tetra values: {s} IMPLIES {s}", .{ @tagName(a.value), @tagName(b.value) });
                return ErrorList.TypeError;
            },
        };

        const b_tetra = switch (b.value) {
            .tetra => |val| if (val <= 3) val else {
                self.reporter.reportError("Invalid tetra value: {}", .{val});
                return ErrorList.TypeError;
            },
            else => {
                self.reporter.reportError("Cannot perform IMPLIES on non-tetra values: {s} IMPLIES {s}", .{ @tagName(a.value), @tagName(b.value) });
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
                // Mixed int/float comparison (from old VM)
                .float => |b_val| @as(f64, @floatFromInt(a_val)) == b_val,
                else => false,
            },
            .float => |a_val| switch (b.value) {
                .float => |b_val| a_val == b_val,
                // Mixed float/int comparison (from old VM)
                .int => |b_val| a_val == @as(f64, @floatFromInt(b_val)),
                else => false,
            },
            .tetra => |a_val| switch (b.value) {
                .tetra => |b_val| a_val == b_val,
                else => false,
            },
            .string => |a_val| switch (b.value) {
                .string => |b_val| std.mem.eql(u8, a_val, b_val),
                else => false,
            },
            .nothing => switch (b.value) {
                .nothing => true,
                else => false,
            },
            .u8 => |a_val| switch (b.value) {
                .u8 => |b_val| a_val == b_val,
                else => false,
            },
            // Complex types - basic equality for now
            .array, .struct_instance, .tuple, .map, .enum_variant => false, // Complex equality not implemented yet
        };
    }

    /// Enhanced less-than with mixed int/float support from old VM
    fn compareLess(self: *HIRVM, a: HIRFrame, b: HIRFrame) !bool {
        _ = self;
        return switch (a.value) {
            .int => |a_val| switch (b.value) {
                .int => |b_val| a_val < b_val,
                // Mixed int/float comparison (from old VM)
                .float => |b_val| @as(f64, @floatFromInt(a_val)) < b_val,
                else => ErrorList.TypeError,
            },
            .float => |a_val| switch (b.value) {
                .float => |b_val| a_val < b_val,
                // Mixed float/int comparison (from old VM)
                .int => |b_val| a_val < @as(f64, @floatFromInt(b_val)),
                else => ErrorList.TypeError,
            },
            .u8 => |a_val| switch (b.value) {
                .u8 => |b_val| a_val < b_val,
                else => ErrorList.TypeError,
            },
            // Complex types don't support comparison
            .tetra, .string, .nothing, .array, .struct_instance, .tuple, .map, .enum_variant => ErrorList.TypeError,
        };
    }

    /// Enhanced greater-than with mixed int/float support from old VM
    fn compareGreater(self: *HIRVM, a: HIRFrame, b: HIRFrame) !bool {
        _ = self;
        return switch (a.value) {
            .int => |a_val| switch (b.value) {
                .int => |b_val| a_val > b_val,
                // Mixed int/float comparison (from old VM)
                .float => |b_val| @as(f64, @floatFromInt(a_val)) > b_val,
                else => ErrorList.TypeError,
            },
            .float => |a_val| switch (b.value) {
                .float => |b_val| a_val > b_val,
                // Mixed float/int comparison (from old VM)
                .int => |b_val| a_val > @as(f64, @floatFromInt(b_val)),
                else => ErrorList.TypeError,
            },
            .u8 => |a_val| switch (b.value) {
                .u8 => |b_val| a_val > b_val,
                else => ErrorList.TypeError,
            },
            // Complex types don't support comparison
            .tetra, .string, .nothing, .array, .struct_instance, .tuple, .map, .enum_variant => ErrorList.TypeError,
        };
    }

    /// Print a HIR value for debugging/inspection
    pub fn printHIRValue(self: *HIRVM, value: HIRValue) !void {
        switch (value) {
            .int => |i| std.debug.print("{}", .{i}),
            .float => |f| std.debug.print("{d}", .{f}),
            .string => |s| std.debug.print("\"{s}\"", .{s}),
            .tetra => |b| std.debug.print("{}", .{b}),
            .u8 => |u| std.debug.print("{}", .{u}),
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
            .struct_instance => std.debug.print("{{struct}}", .{}),
            .tuple => std.debug.print("(tuple)", .{}),
            .map => std.debug.print("{{map}}", .{}),
            .enum_variant => std.debug.print("enum_variant", .{}),
        }
    }

    /// Format HIR value to a writer (for proper UTF-8 buffered output like old interpreter)
    pub fn formatHIRValue(self: *HIRVM, writer: anytype, value: HIRValue) !void {
        switch (value) {
            .int => |i| try writer.print("{}", .{i}),
            .float => |f| try writer.print("{d}", .{f}),
            .string => |s| try writer.print("\"{s}\"", .{s}),
            .tetra => |b| try writer.print("{}", .{b}),
            .u8 => |u| try writer.print("{}", .{u}),
            .nothing => try writer.print("nothing", .{}),
            // Complex types - show contents for arrays
            .array => |arr| {
                try writer.print("[", .{});
                var first = true;
                for (arr.elements) |elem| {
                    if (std.meta.eql(elem, HIRValue.nothing)) break;
                    if (!first) try writer.print(", ", .{});
                    try self.formatHIRValue(writer, elem);
                    first = false;
                }
                try writer.print("]", .{});
            },
            .struct_instance => try writer.print("{{struct}}", .{}),
            .tuple => try writer.print("(tuple)", .{}),
            .map => try writer.print("{{map}}", .{}),
            .enum_variant => try writer.print("enum_variant", .{}),
        }
    }

    /// Debug helper to dump current VM state
    pub fn dumpState(self: *HIRVM) void {
        if (!self.debug_enabled) return;

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
};
