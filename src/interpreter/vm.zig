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
pub fn hirValueToTokenLiteral(hir_value: HIRValue) TokenLiteral {
    return switch (hir_value) {
        .int => |i| TokenLiteral{ .int = i },
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
        .u8 => |u| TokenLiteral{ .u8 = u },
        .enum_variant => |e| TokenLiteral{ .enum_variant = e.variant_name },
        .array => |arr| blk: {
            var token_elements = std.ArrayList(TokenLiteral).init(std.heap.page_allocator);
            defer token_elements.deinit();

            for (arr.elements) |elem| {
                if (std.meta.eql(elem, HIRValue.nothing)) break;
                // Ignore append errors - just return empty array if allocation fails
                token_elements.append(hirValueToTokenLiteral(elem)) catch break :blk TokenLiteral{ .nothing = {} };
            }

            break :blk TokenLiteral{ .array = token_elements.toOwnedSlice() catch &[_]TokenLiteral{} };
        },
        .struct_instance => |s| blk: {
            var fields = std.ArrayList(StructField).init(std.heap.page_allocator);
            defer fields.deinit();

            for (s.fields) |field| {
                // Create a new path for the field by appending its name to the current path
                const field_path = if (s.path) |path| blk2: {
                    const new_path = std.fmt.allocPrint(std.heap.page_allocator, "{s}.{s}", .{ path, field.name }) catch break :blk2 field.name;
                    break :blk2 new_path;
                } else field.name;

                // Create a new HIRValue with the field's path and name
                var field_value = field.value;
                if (field_value == .struct_instance) {
                    var new_struct = field_value.struct_instance;
                    new_struct.path = field_path;
                    new_struct.field_name = field.name;
                    field_value = HIRValue{ .struct_instance = new_struct };
                }

                // Ignore append errors - just return empty struct if allocation fails
                fields.append(.{
                    .name = field.name,
                    .value = hirValueToTokenLiteral(field_value),
                }) catch continue;
            }

            const fields_slice = fields.toOwnedSlice() catch &[_]StructField{};
            const mutable_slice = std.heap.page_allocator.alloc(StructField, fields_slice.len) catch break :blk TokenLiteral{ .nothing = {} };
            @memcpy(mutable_slice, fields_slice);
            const result = TokenLiteral{ .struct_value = .{
                .type_name = s.type_name,
                .fields = mutable_slice,
                .path = s.path,
            } };
            break :blk result;
        },
        .tuple => |t| blk: {
            var token_elements = std.ArrayList(TokenLiteral).init(std.heap.page_allocator);
            defer token_elements.deinit();

            for (t.elements) |elem| {
                // Ignore append errors - just return empty tuple if allocation fails
                token_elements.append(hirValueToTokenLiteral(elem)) catch break;
            }

            break :blk TokenLiteral{ .tuple = token_elements.toOwnedSlice() catch &[_]TokenLiteral{} };
        },
        .map => |m| blk: {
            var token_map = std.StringHashMap(TokenLiteral).init(std.heap.page_allocator);

            for (m.entries) |entry| {
                // Convert key to string (maps expect string keys)
                const key_str = switch (entry.key) {
                    .string => |s| s,
                    .int => |i| std.fmt.allocPrint(std.heap.page_allocator, "{d}", .{i}) catch continue,
                    else => continue, // Skip non-string, non-int keys
                };

                const token_value = hirValueToTokenLiteral(entry.value);
                token_map.put(key_str, token_value) catch continue;
            }

            break :blk TokenLiteral{ .map = token_map };
        },
    };
}

pub fn hirValueToTokenType(hir_value: HIRValue) TokenType {
    return switch (hir_value) {
        .int => .INT,
        .u8 => .U8,
        .float => .FLOAT,
        .string => .STRING,
        .tetra => .TETRA,
        .nothing => .NOTHING,
        // Complex types - map to closest TokenType
        .array => .ARRAY,
        .struct_instance => .STRUCT, // Structs represented as struct values
        .tuple => .ARRAY, // Tuples similar to arrays
        .map => .ARRAY, // Maps similar to arrays
        .enum_variant => .IDENTIFIER, // Enums represented as identifiers
    };
}

pub fn hirValueToTypeInfo(hir_value: HIRValue) TypeInfo {
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

/// HIR-based Stack - uses heap allocation to avoid system stack overflow
const HIRStack = struct {
    stack: []HIRFrame,
    sp: u32,
    allocator: std.mem.Allocator,

    pub fn init(allocator: std.mem.Allocator, size: u32) !HIRStack {
        const stack = try allocator.alloc(HIRFrame, size);
        return HIRStack{
            .stack = stack,
            .sp = 0,
            .allocator = allocator,
        };
    }

    pub fn deinit(self: *HIRStack) void {
        self.allocator.free(self.stack);
    }

    pub fn push(self: *HIRStack, value: HIRFrame) !void {
        if (self.sp >= self.stack.len) {
            return error.StackOverflow;
        }
        self.stack[self.sp] = value;
        self.sp += 1;
    }

    pub fn pop(self: *HIRStack) !HIRFrame {
        if (self.sp == 0) {
            return error.StackUnderflow;
        }
        self.sp -= 1;
        return self.stack[self.sp];
    }

    pub fn peek(self: *HIRStack) ?HIRFrame {
        if (self.sp == 0) {
            return null;
        }
        return self.stack[self.sp - 1];
    }

    pub fn clear(self: *HIRStack) void {
        self.sp = 0;
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

    // Execution state
    ip: u32 = 0, // Instruction pointer (index into instructions array)
    stack: HIRStack,
    call_stack: CallStack, // Track function calls and returns
    current_scope: *Scope, // Current execution scope
    running: bool = true,
    current_field_name: ?[]const u8 = null, // Track current field name for struct access

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

    fn tokenLiteralToHIRValue(self: *HIRVM, token_literal: TokenLiteral) HIRValue {
        const allocator = self.memory_manager.getAllocator();
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
            // Convert TokenLiteral arrays back to HIR arrays
            .array => |arr| blk: {
                // CRITICAL FIX: For empty arrays, allocate minimum capacity for growth
                const min_capacity = if (arr.len == 0) 8 else arr.len;
                const elements = allocator.alloc(HIRValue, min_capacity) catch {
                    // On allocation failure, return empty array
                    break :blk HIRValue{
                        .array = HIRArray{
                            .elements = &[_]HIRValue{},
                            .element_type = .Auto,
                            .capacity = 0,
                        },
                    };
                };

                // Initialize all elements to nothing
                for (elements) |*elem| {
                    elem.* = HIRValue.nothing;
                }

                // Copy actual elements
                for (arr, 0..) |elem, i| {
                    if (i >= arr.len) break;
                    elements[i] = self.tokenLiteralToHIRValue(elem);
                }

                break :blk HIRValue{
                    .array = HIRArray{
                        .elements = elements,
                        .element_type = .Auto, // Infer from first element
                        .capacity = @intCast(min_capacity), // CRITICAL FIX: Use min_capacity, not arr.len
                    },
                };
            },
            // Convert TokenLiteral tuples back to HIR tuples
            .tuple => |t| blk: {
                const elements = allocator.alloc(HIRValue, t.len) catch {
                    break :blk HIRValue.nothing;
                };

                for (t, 0..) |elem, i| {
                    elements[i] = self.tokenLiteralToHIRValue(elem);
                }

                break :blk HIRValue{
                    .tuple = HIRTuple{
                        .elements = elements,
                    },
                };
            },
            // Convert TokenLiteral maps back to HIR maps
            .map => |m| blk: {
                // Count entries in the hashmap
                var count: usize = 0;
                var iterator = m.iterator();
                while (iterator.next()) |_| {
                    count += 1;
                }

                const entries = allocator.alloc(HIRMapEntry, count) catch {
                    break :blk HIRValue.nothing;
                };

                // Fill entries array
                var i: usize = 0;
                iterator = m.iterator();
                while (iterator.next()) |entry| {
                    entries[i] = HIRMapEntry{
                        .key = HIRValue{ .string = entry.key_ptr.* },
                        .value = self.tokenLiteralToHIRValue(entry.value_ptr.*),
                    };
                    i += 1;
                }

                break :blk HIRValue{
                    .map = HIRMap{
                        .entries = entries,
                        .key_type = .String,
                        .value_type = .Auto, // Infer from values
                    },
                };
            },
            // Other complex types still convert to nothing
            .struct_value => |s| blk: {
                // Allocate fields array
                const fields = allocator.alloc(HIRStructField, s.fields.len) catch {
                    break :blk HIRValue.nothing;
                };

                // Convert each field
                for (s.fields, 0..) |field, i| {
                    fields[i] = HIRStructField{
                        .name = field.name,
                        .value = self.tokenLiteralToHIRValue(field.value),
                        .field_type = .Auto, // Default to Auto, will be inferred from value
                    };
                }

                // Create struct instance with proper field tracking
                break :blk HIRValue{
                    .struct_instance = HIRStruct{
                        .type_name = s.type_name,
                        .fields = fields,
                        .field_name = null, // Base struct has no field name
                        .path = null, // Base struct has no path initially
                    },
                };
            },
            .function => HIRValue.nothing,
            .enum_variant => HIRValue.nothing,
        };
    }

    fn pop(self: *HIRVM) HIRFrame {
        return self.stack.pop() catch unreachable;
    }

    fn popString(self: *HIRVM) ![]const u8 {
        const value = try self.stack.pop();
        return value.asString();
    }

    fn didJump(self: *HIRVM, instruction: HIRInstruction) bool {
        _ = self; // Not used currently
        return switch (instruction) {
            .Jump => true,
            .JumpCond => true,
            .Call => |c| switch (c.call_kind) {
                .LocalFunction => true,
                else => false,
            },
            .TailCall => true,
            .Return => true,
            else => false,
        };
    }

    fn formatHIRValue(self: *HIRVM, writer: anytype, value: HIRValue) !void {
        self.reporter.debug("Formatting HIR value: {any}", .{value});
        switch (value) {
            .int => |i| {
                self.reporter.debug("Formatting integer: {d}", .{i});
                try writer.print("{}", .{i});
            },
            .float => |f| {
                self.reporter.debug("Formatting float: {d}", .{f});
                try writer.print("{d}", .{f});
            },
            .string => |s| {
                self.reporter.debug("Formatting string: {s}", .{s});
                try writer.print("\"{s}\"", .{s});
            },
            .nothing => {
                self.reporter.debug("Formatting nothing", .{});
                try writer.print("nothing", .{});
            },
            .tetra => |b| {
                self.reporter.debug("Formatting tetra: {}", .{b});
                try writer.print("{}", .{b});
            },
            .array => |a| {
                self.reporter.debug("Formatting array with {d} elements", .{a.elements.len});
                try writer.print("[", .{});
                for (a.elements, 0..) |element, i| {
                    try self.formatHIRValue(writer, element);
                    if (i < a.elements.len - 1) {
                        try writer.print(", ", .{});
                    }
                }
                try writer.print("]", .{});
            },
            .struct_instance => |s| {
                self.reporter.debug("Formatting struct instance of type '{s}' with {d} fields", .{ s.type_name, s.fields.len });
                // Don't print the struct type name for nested structs
                if (s.path == null) {
                    try writer.print("{s} {{ ", .{s.type_name});
                } else {
                    try writer.print("{{ ", .{});
                }
                for (s.fields, 0..) |field, i| {
                    try writer.print("{s}: ", .{field.name});
                    try self.formatHIRValue(writer, field.value);
                    if (i < s.fields.len - 1) {
                        try writer.print(", ", .{});
                    }
                }
                try writer.print(" }}", .{});
            },
            .tuple => |t| {
                self.reporter.debug("Formatting tuple with {d} elements", .{t.elements.len});
                try writer.print("(", .{});
                for (t.elements, 0..) |element, i| {
                    try self.formatHIRValue(writer, element);
                    if (i < t.elements.len - 1) {
                        try writer.print(", ", .{});
                    }
                }
                try writer.print(")", .{});
            },
            else => {
                self.reporter.debug("Formatting unknown value type: {s}", .{@tagName(value)});
                try writer.print("<{s}>", .{@tagName(value)});
            },
        }
    }

    pub fn init(program: *HIRProgram, reporter: *Reporter, memory_manager: *MemoryManager) !HIRVM {
        const allocator = memory_manager.getAllocator();

        // Create a new execution scope for this HIR VM run
        // This allows proper cleanup and isolation
        const execution_scope = try memory_manager.scope_manager.createScope(memory_manager.scope_manager.root_scope);

        var vm = HIRVM{
            .program = program,
            .reporter = reporter,
            .memory_manager = memory_manager,
            .allocator = allocator, // Use the passed allocator (memory manager's arena)
            .stack = try HIRStack.init(allocator, STACK_SIZE), // Use the passed allocator
            .call_stack = try CallStack.init(allocator), // Use the passed allocator
            .current_scope = execution_scope,
            .label_map = std.StringHashMap(u32).init(allocator), // Use the passed allocator
            .var_cache = std.StringHashMap(u32).init(allocator), // Use the passed allocator
            .fast_mode = true,
            .turbo_mode = true,
            .hot_vars = [_]?HotVar{null} ** 4,
            .hot_var_count = 0,
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

            // OPTIMIZED: Reduce debug output in fast mode for better performance
            if (!self.fast_mode or self.ip < 10) {
                self.reporter.debug("HIR VM [{}]: Executing {s}\n", .{ self.ip, @tagName(instruction) });
            }

            try self.executeInstruction(instruction);

            // Only advance IP if instruction didn't jump
            if (!self.didJump(instruction)) {
                self.ip += 1;
            }
        }

        // Return final result from stack if available
        if (self.stack.sp > 0) {
            return try self.stack.pop();
        }

        return null;
    }

    /// CRITICAL: Validate that no Auto types exist in the HIR program
    fn validateHIRProgram(self: *HIRVM) !void {
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
                        return ErrorList.InternalParserError;
                    }
                },
                .Convert => |c| {
                    if (c.from_type == .Auto or c.to_type == .Auto) {
                        self.reporter.reportError("FATAL: Auto type found in Convert instruction at IP {} (from: {s}, to: {s}). Type inference likely failed during code generation.", .{ ip, @tagName(c.from_type), @tagName(c.to_type) });
                        return ErrorList.InternalParserError;
                    }
                },
                .Call => |c| {
                    if (c.return_type == .Auto) {
                        self.reporter.reportError("FATAL: Auto return_type found in Call instruction at IP {} for function '{s}'. Type inference likely failed during code generation.", .{ ip, c.qualified_name });
                        return ErrorList.InternalParserError;
                    }
                },
                .TailCall => |c| {
                    if (c.return_type == .Auto) {
                        self.reporter.reportError("FATAL: Auto return_type found in TailCall instruction at IP {} for function '{s}'. Type inference likely failed during code generation.", .{ ip, c.qualified_name });
                        return ErrorList.InternalParserError;
                    }
                },
                .Return => |r| {
                    if (r.return_type == .Auto) {
                        self.reporter.reportError("FATAL: Auto return_type found in Return instruction at IP {}. Type inference likely failed during code generation.", .{ip});
                        return ErrorList.InternalParserError;
                    }
                },
                else => {
                    // Other instructions don't have explicit type fields to validate
                },
            }
        }

        self.reporter.debug(">> SANITY CHECK PASSED: No Auto types found in HIR program\n", .{});
    }

    /// Execute a single HIR instruction
    fn executeInstruction(self: *HIRVM, instruction: HIRInstruction) !void {
        switch (instruction) {
            .Const => |c| {
                // Push constant from constant pool
                const constant_value = self.program.constant_pool[c.constant_id];
                try self.stack.push(HIRFrame.initFromHIRValue(constant_value));
            },

            .StructNew => |s| {
                var fields = std.ArrayList(hir_values.HIRStructField).init(self.allocator);
                defer fields.deinit();

                // Fields are pushed in reverse order, so we need to process them in reverse
                var field_values = try self.allocator.alloc(HIRFrame, s.field_count);
                defer self.allocator.free(field_values);
                var field_names = try self.allocator.alloc([]const u8, s.field_count);
                defer self.allocator.free(field_names);

                // First pop all values and names
                // HIR generation pushes field_value first, then field_name, so we pop field_name first
                var i: usize = 0;
                while (i < s.field_count) : (i += 1) {
                    const field_name = try self.stack.pop();
                    const field_value = try self.stack.pop();

                    // Debug output removed for cleaner execution

                    field_names[s.field_count - 1 - i] = try field_name.asString(); // Store in reverse
                    field_values[s.field_count - 1 - i] = field_value; // Store in reverse
                }

                // Now process them in the correct order
                for (0..s.field_count) |idx| {
                    const field_name = field_names[idx];
                    const field_value = field_values[idx];

                    // Validate field type
                    const expected_type = s.field_types[idx];
                    const actual_type = switch (field_value.value) {
                        .int => HIRType.Int,
                        .float => HIRType.Float,
                        .string => HIRType.String,
                        .tetra => HIRType.Tetra,
                        .array => HIRType.Array,
                        .struct_instance => HIRType.Struct,
                        .tuple => HIRType.Tuple,
                        .map => HIRType.Map,
                        .nothing => HIRType.Nothing,
                        else => HIRType.Auto,
                    };

                    if (expected_type != actual_type and expected_type != .Auto) {
                        return self.reporter.reportError("Type mismatch for field '{s}' in struct '{s}'. Expected {s}, got {s}", .{
                            field_name,
                            s.type_name,
                            @tagName(expected_type),
                            @tagName(actual_type),
                        });
                    }

                    // Create field with proper path tracking
                    var field_value_to_store = field_value.value;
                    if (field_value.value == .struct_instance) {
                        var new_struct = field_value.value.struct_instance;
                        new_struct.field_name = field_name;
                        // For nested structs, create a path that includes the field name
                        new_struct.path = try self.allocator.dupe(u8, field_name);
                        field_value_to_store = HIRValue{ .struct_instance = new_struct };
                    }

                    try fields.append(.{
                        .name = field_name,
                        .value = field_value_to_store,
                        .field_type = expected_type,
                    });
                }

                // Create struct instance with proper path tracking
                const struct_instance = HIRValue{
                    .struct_instance = .{
                        .type_name = s.type_name,
                        .fields = try fields.toOwnedSlice(),
                        .field_name = null, // Base struct has no field name
                        .path = null, // Base struct has no path initially
                    },
                };

                var frame = HIRFrame{ .value = struct_instance };
                frame.field_name = null; // Base struct has no field name
                try self.stack.push(frame);
            },

            .LoadVar => |v| {
                if (self.turbo_mode) {
                    // TURBO: Check hot variable cache first (array lookup - fastest possible)
                    for (self.hot_vars[0..self.hot_var_count]) |hot_var| {
                        if (hot_var != null and std.mem.eql(u8, hot_var.?.name, v.var_name)) {
                            if (self.memory_manager.scope_manager.value_storage.get(hot_var.?.storage_id)) |storage| {
                                const hir_value = self.tokenLiteralToHIRValue(storage.value);
                                self.reporter.debug(">>   TURBO hit for '{s}' = {any} (storage_id: {})\n", .{ v.var_name, hir_value, hot_var.?.storage_id });
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
                            self.reporter.debug(">>   CACHE hit for '{s}' = {any} (storage_id: {})\n", .{ v.var_name, hir_value, cached_storage_id });
                            try self.stack.push(HIRFrame.initFromHIRValue(hir_value));
                            return; // Ultra-fast cached path
                        } else {
                            // Cache is stale, remove entry
                            self.reporter.debug(">>   CACHE stale for '{s}' (storage_id: {})\n", .{ v.var_name, cached_storage_id });
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

                        try self.stack.push(HIRFrame.initFromHIRValue(hir_value));
                    } else {
                        return self.reporter.reportError("Undefined variable: {s}", .{v.var_name});
                    }
                } else {
                    return self.reporter.reportError("Undefined variable: {s}", .{v.var_name});
                }
            },

            .StoreVar => |v| {
                const value = try self.stack.pop();

                // Initialize the path with the variable name for struct instances
                var store_value = value.value;
                if (store_value == .struct_instance) {
                    var new_struct = store_value.struct_instance;
                    new_struct.path = try self.allocator.dupe(u8, v.var_name);
                    store_value = HIRValue{ .struct_instance = new_struct };
                }

                const token_literal = hirValueToTokenLiteral(store_value);
                const token_type = hirValueToTokenType(store_value);
                const type_info = hirValueToTypeInfo(store_value);

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

                        storage.*.value = token_literal;
                        storage.*.type = token_type;
                        storage.*.type_info = type_info;
                    } else {
                        return self.reporter.reportError("Variable storage not found for: {s}", .{v.var_name});
                    }
                } else {
                    // Variable doesn't exist in current scope - create NEW variable with NEW storage
                    // This ensures function parameters get their own storage in each recursive call

                    _ = self.current_scope.createValueBinding(v.var_name, token_literal, token_type, type_info, false) catch |err| {
                        return self.reporter.reportError("Failed to create variable {s}: {}", .{ v.var_name, err });
                    };
                }

                // Push the value back onto the stack with field names preserved
                var frame = HIRFrame.initFromHIRValue(store_value);
                frame.field_name = value.field_name;
                try self.stack.push(frame);
            },

            .StoreConst => |c| {
                const value = try self.stack.pop();
                const token_literal = hirValueToTokenLiteral(value.value);
                const token_type = hirValueToTokenType(value.value);
                const type_info = hirValueToTypeInfo(value.value);

                _ = self.current_scope.createValueBinding(c.var_name, token_literal, token_type, type_info, true) catch |err| {
                    return self.reporter.reportError("Failed to create constant {s}: {}", .{ c.var_name, err });
                };
            },

            .IntArith => |a| {
                const b = try self.stack.pop();
                const a_val = try self.stack.pop();

                const result = switch (a.op) {
                    .Add => try a_val.asInt() + try b.asInt(),
                    .Sub => try a_val.asInt() - try b.asInt(),
                    .Mul => try a_val.asInt() * try b.asInt(),
                    .Div => @divTrunc(try a_val.asInt(), try b.asInt()),
                    .Mod => @mod(try a_val.asInt(), try b.asInt()),
                };

                try self.stack.push(HIRFrame.initInt(result));
            },

            .FloatArith => |a| {
                const b = try self.stack.pop();
                const a_val = try self.stack.pop();

                const result = switch (a.op) {
                    .Add => try a_val.asFloat() + try b.asFloat(),
                    .Sub => try a_val.asFloat() - try b.asFloat(),
                    .Mul => try a_val.asFloat() * try b.asFloat(),
                    .Div => try a_val.asFloat() / try b.asFloat(),
                    .Mod => return ErrorList.UnsupportedOperator,
                };

                try self.stack.push(HIRFrame.initFloat(result));
            },

            .Convert => |c| {
                const value = try self.stack.pop();
                const result = switch (c.from_type) {
                    .Int => switch (c.to_type) {
                        .Float => HIRFrame.initFloat(@as(f64, @floatFromInt(try value.asInt()))),
                        .String => HIRFrame.initString(try std.fmt.allocPrint(self.allocator, "{d}", .{try value.asInt()})),
                        else => return ErrorList.TypeError,
                    },
                    .Float => switch (c.to_type) {
                        .Int => HIRFrame.initInt(@as(i32, @intFromFloat(try value.asFloat()))),
                        .String => HIRFrame.initString(try std.fmt.allocPrint(self.allocator, "{d}", .{try value.asFloat()})),
                        else => return ErrorList.TypeError,
                    },
                    .String => switch (c.to_type) {
                        .Int => blk: {
                            const str = try value.asString();
                            const parsed = std.fmt.parseInt(i32, str, 10) catch break :blk HIRFrame.initNothing();
                            break :blk HIRFrame.initInt(parsed);
                        },
                        .Float => blk: {
                            const str = try value.asString();
                            const parsed = std.fmt.parseFloat(f64, str) catch break :blk HIRFrame.initNothing();
                            break :blk HIRFrame.initFloat(parsed);
                        },
                        else => return ErrorList.TypeError,
                    },
                    else => return ErrorList.TypeError,
                };
                try self.stack.push(result);
            },

            .Compare => |c| {
                const b = try self.stack.pop();
                const a = try self.stack.pop();

                const result = switch (c.op) {
                    .Eq => try self.compareEqual(a, b),
                    .Ne => !(try self.compareEqual(a, b)),
                    .Lt => try self.compareLess(a, b),
                    .Le => (try self.compareLess(a, b)) or (try self.compareEqual(a, b)),
                    .Gt => try self.compareGreater(a, b),
                    .Ge => (try self.compareGreater(a, b)) or (try self.compareEqual(a, b)),
                };

                try self.stack.push(HIRFrame.initTetra(if (result) 1 else 0));
            },

            .LogicalOp => |l| {
                const b = try self.stack.pop();
                const a = try self.stack.pop();

                const result = switch (l.op) {
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

            .Jump => |j| {
                if (self.label_map.get(j.label)) |target_ip| {
                    self.ip = target_ip;
                } else {
                    return self.reporter.reportError("Unknown label: {s}", .{j.label});
                }
            },

            .JumpCond => |j| {
                const condition = try self.stack.pop();
                const should_jump = switch (condition.value) {
                    .tetra => |t| t == 1,
                    .int => |i| i != 0,
                    .float => |f| f != 0.0,
                    else => false,
                };

                const target_label = if (should_jump) j.label_true else j.label_false;
                if (self.label_map.get(target_label)) |target_ip| {
                    self.ip = target_ip;
                } else {
                    return self.reporter.reportError("Unknown label: {s}", .{target_label});
                }
            },

            .Label => {
                // Labels are no-ops during execution
            },

            .Call => |c| {
                // Handle function calls
                switch (c.call_kind) {
                    .LocalFunction => {
                        if (c.function_index >= self.program.function_table.len) {
                            return self.reporter.reportError("Invalid function index: {} (max: {})", .{ c.function_index, self.program.function_table.len });
                        }

                        const function = self.program.function_table[c.function_index];
                        const return_ip = self.ip + 1;

                        const call_frame = CallFrame{
                            .return_ip = return_ip,
                            .function_name = function.name,
                            .arg_count = c.arg_count,
                        };
                        try self.call_stack.push(call_frame);

                        if (self.label_map.get(function.start_label)) |target_ip| {
                            self.ip = target_ip;
                        } else {
                            return self.reporter.reportError("Function label not found: {s}", .{function.start_label});
                        }
                    },
                    .BuiltinFunction => {
                        // Handle built-in functions
                        if (std.mem.eql(u8, c.qualified_name, "print")) {
                            const value = try self.stack.pop();
                            try self.formatHIRValue(std.io.getStdOut().writer(), value.value);
                            try std.io.getStdOut().writer().writeByte('\n');
                        } else {
                            return self.reporter.reportError("Unknown built-in function: {s}", .{c.qualified_name});
                        }
                    },
                    .ModuleFunction => {
                        // Module functions not implemented yet
                        return self.reporter.reportError("Module functions not implemented yet: {s}", .{c.qualified_name});
                    },
                }
            },

            .TailCall => |c| {
                switch (c.call_kind) {
                    .LocalFunction => {
                        if (c.function_index >= self.program.function_table.len) {
                            return self.reporter.reportError("Invalid function index: {} (max: {})", .{ c.function_index, self.program.function_table.len });
                        }

                        const function = self.program.function_table[c.function_index];
                        if (self.label_map.get(function.start_label)) |target_ip| {
                            self.ip = target_ip;
                        } else {
                            return self.reporter.reportError("Function label not found: {s}", .{function.start_label});
                        }
                    },
                    else => return self.reporter.reportError("Tail call not supported for call kind: {s}", .{@tagName(c.call_kind)}),
                }
            },

            .Return => |r| {
                if (!r.has_value) {
                    try self.stack.push(HIRFrame.initNothing());
                }

                if (self.call_stack.isEmpty()) {
                    self.running = false;
                } else {
                    const call_frame = try self.call_stack.pop();
                    self.ip = call_frame.return_ip;
                }
            },

            .GetField => |f| {
                const value = try self.stack.pop();
                self.reporter.debug("GetField called for field '{s}' on value: {any}", .{ f.field_name, value });

                switch (value.value) {
                    .struct_instance => |s| {
                        self.reporter.debug("Looking up field '{s}' in struct of type '{s}'", .{ f.field_name, s.type_name });
                        for (s.fields) |field| {
                            if (std.mem.eql(u8, field.name, f.field_name)) {
                                self.reporter.debug("Found field '{s}' with value: {any}", .{ field.name, field.value });
                                var frame = HIRFrame.initFromHIRValue(field.value);
                                frame.field_name = field.name;
                                try self.stack.push(frame);
                                return;
                            }
                        }
                        return self.reporter.reportError("Field not found: {s}", .{f.field_name});
                    },
                    else => return self.reporter.reportError("Cannot access field '{s}' on non-struct value", .{f.field_name}),
                }
            },

            .SetField => |f| {
                const new_value = try self.stack.pop();
                const target = try self.stack.pop();
                self.reporter.debug("SetField called for field '{s}' with new value: {any}", .{ f.field_name, new_value });

                switch (target.value) {
                    .struct_instance => |*s| {
                        self.reporter.debug("Setting field '{s}' in struct of type '{s}'", .{ f.field_name, s.type_name });
                        for (s.fields) |*field| {
                            if (std.mem.eql(u8, field.name, f.field_name)) {
                                self.reporter.debug("Found field '{s}', updating value from {any} to {any}", .{ field.name, field.value, new_value.value });
                                field.value = new_value.value;
                                try self.stack.push(target);
                                return;
                            }
                        }
                        return self.reporter.reportError("Field not found: {s}", .{f.field_name});
                    },
                    else => return self.reporter.reportError("Cannot set field '{s}' on non-struct value", .{f.field_name}),
                }
            },

            .StoreFieldName => |s| {
                // Get the top value on the stack without popping it
                if (self.stack.peek()) |top| {
                    if (top.value == .struct_instance) {
                        var new_struct = top.value.struct_instance;
                        // Update field_name
                        new_struct.field_name = s.field_name;

                        // Build or extend the path properly for nested field access
                        if (new_struct.path) |existing_path| {
                            // Extend existing path: e.g., "mike.person" + "age" = "mike.person.age"
                            const new_path = try std.fmt.allocPrint(self.allocator, "{s}.{s}", .{ existing_path, s.field_name });
                            new_struct.path = new_path;
                        } else {
                            // Start a new path with just the field name
                            new_struct.path = try self.allocator.dupe(u8, s.field_name);
                        }

                        // Update the value on the stack
                        _ = try self.stack.pop();
                        var frame = HIRFrame.initFromHIRValue(HIRValue{ .struct_instance = new_struct });
                        frame.field_name = s.field_name;
                        try self.stack.push(frame);
                    } else {
                        // For non-struct values, just store the field name
                        var frame = HIRFrame.initFromHIRValue(top.value);
                        frame.field_name = s.field_name;
                        _ = try self.stack.pop();
                        try self.stack.push(frame);
                    }
                }
                self.current_field_name = s.field_name;
            },

            .TryBegin, .TryCatch, .Throw => {
                // Exception handling not implemented yet
                return ErrorList.NotImplemented;
            },

            .EnterScope => {
                const new_scope = try self.memory_manager.scope_manager.createScope(self.current_scope);
                self.current_scope = new_scope;
                if (self.fast_mode) {
                    self.var_cache.clearRetainingCapacity();
                }
            },

            .ExitScope => {
                if (self.current_scope.parent) |parent_scope| {
                    const old_scope = self.current_scope;
                    self.current_scope = parent_scope;
                    if (self.fast_mode) {
                        self.var_cache.clearRetainingCapacity();
                    }
                    old_scope.deinit();
                }
            },

            .ArrayNew => |a| {
                const elements = try self.allocator.alloc(HIRValue, a.size);
                for (elements) |*element| {
                    element.* = HIRValue.nothing;
                }
                const array = HIRValue{ .array = HIRArray{
                    .elements = elements,
                    .element_type = a.element_type,
                    .capacity = a.size,
                } };
                try self.stack.push(HIRFrame.initFromHIRValue(array));
            },

            .ArrayGet => {
                const index = try self.stack.pop();
                const array = try self.stack.pop();

                switch (array.value) {
                    .array => |arr| {
                        const idx = switch (index.value) {
                            .int => |i| if (i < 0) return ErrorList.IndexOutOfBounds else @as(usize, @intCast(i)),
                            else => return ErrorList.TypeError,
                        };
                        if (idx >= arr.elements.len) {
                            return ErrorList.IndexOutOfBounds;
                        }
                        try self.stack.push(HIRFrame.initFromHIRValue(arr.elements[idx]));
                    },
                    else => return ErrorList.TypeError,
                }
            },

            .ArraySet => {
                const value = try self.stack.pop();
                const index = try self.stack.pop();
                const array = try self.stack.pop();

                switch (array.value) {
                    .array => |*arr| {
                        const idx = switch (index.value) {
                            .int => |i| if (i < 0) return ErrorList.IndexOutOfBounds else @as(usize, @intCast(i)),
                            else => return ErrorList.TypeError,
                        };
                        if (idx >= arr.elements.len) {
                            return ErrorList.IndexOutOfBounds;
                        }
                        arr.elements[idx] = value.value;
                        try self.stack.push(HIRFrame.initFromHIRValue(array.value));
                    },
                    else => return ErrorList.TypeError,
                }
            },

            .ArrayPush => {
                const value = try self.stack.pop();
                const array = try self.stack.pop();

                switch (array.value) {
                    .array => |arr| {
                        var length: usize = 0;
                        for (arr.elements) |elem| {
                            if (std.meta.eql(elem, HIRValue.nothing)) break;
                            length += 1;
                        }
                        if (length >= arr.capacity) {
                            const new_capacity = arr.capacity * 2;
                            // Create a new array with increased capacity
                            const new_elements = try self.allocator.alloc(HIRValue, new_capacity);
                            // Copy existing elements
                            @memcpy(new_elements[0..arr.elements.len], arr.elements);
                            // Initialize remaining elements to nothing
                            for (new_elements[arr.elements.len..]) |*elem| {
                                elem.* = HIRValue.nothing;
                            }
                            // Create a new array value with the new elements
                            const new_array = HIRValue{ .array = HIRArray{
                                .elements = new_elements,
                                .element_type = arr.element_type,
                                .capacity = new_capacity,
                            } };
                            // Add the new value
                            new_elements[length] = value.value;
                            // Push the new array onto the stack
                            try self.stack.push(HIRFrame.initFromHIRValue(new_array));
                            return;
                        }
                        // If we didn't need to resize, create a new array value with modified elements
                        var new_elements = try self.allocator.alloc(HIRValue, arr.capacity);
                        @memcpy(new_elements[0..arr.elements.len], arr.elements);
                        new_elements[length] = value.value;
                        const new_array = HIRValue{ .array = HIRArray{
                            .elements = new_elements,
                            .element_type = arr.element_type,
                            .capacity = arr.capacity,
                        } };
                        try self.stack.push(HIRFrame.initFromHIRValue(new_array));
                    },
                    else => return ErrorList.TypeError,
                }
            },

            .ArrayPop => {
                const array = try self.stack.pop();

                switch (array.value) {
                    .array => |*arr| {
                        var length: usize = 0;
                        for (arr.elements) |elem| {
                            if (std.meta.eql(elem, HIRValue.nothing)) break;
                            length += 1;
                        }
                        if (length == 0) {
                            return ErrorList.IndexOutOfBounds;
                        }
                        const value = arr.elements[length - 1];
                        arr.elements[length - 1] = HIRValue.nothing;
                        try self.stack.push(HIRFrame.initFromHIRValue(value));
                    },
                    else => return ErrorList.TypeError,
                }
            },

            .ArrayLen => {
                const array = try self.stack.pop();

                switch (array.value) {
                    .array => |arr| {
                        var length: usize = 0;
                        for (arr.elements) |elem| {
                            if (std.meta.eql(elem, HIRValue.nothing)) break;
                            length += 1;
                        }
                        try self.stack.push(HIRFrame.initInt(@as(i32, @intCast(length))));
                    },
                    else => return ErrorList.TypeError,
                }
            },

            .ArrayConcat => {
                const b = try self.stack.pop();
                const a = try self.stack.pop();

                switch (a.value) {
                    .array => |arr_a| {
                        switch (b.value) {
                            .array => |arr_b| {
                                var length_a: usize = 0;
                                for (arr_a.elements) |elem| {
                                    if (std.meta.eql(elem, HIRValue.nothing)) break;
                                    length_a += 1;
                                }
                                var length_b: usize = 0;
                                for (arr_b.elements) |elem| {
                                    if (std.meta.eql(elem, HIRValue.nothing)) break;
                                    length_b += 1;
                                }
                                const total_capacity = @as(u32, @intCast(@min(length_a + length_b, std.math.maxInt(u32))));
                                const elements = try self.allocator.alloc(HIRValue, total_capacity);
                                @memcpy(elements[0..length_a], arr_a.elements[0..length_a]);
                                @memcpy(elements[length_a..], arr_b.elements[0..length_b]);
                                const array = HIRValue{ .array = HIRArray{
                                    .elements = elements,
                                    .element_type = arr_a.element_type,
                                    .capacity = total_capacity,
                                } };
                                try self.stack.push(HIRFrame.initFromHIRValue(array));
                            },
                            else => return ErrorList.TypeError,
                        }
                    },
                    else => return ErrorList.TypeError,
                }
            },

            .TupleNew => |t| {
                const elements = try self.allocator.alloc(HIRValue, t.element_count);
                var i: usize = 0;
                while (i < t.element_count) : (i += 1) {
                    const value = try self.stack.pop();
                    elements[i] = value.value;
                }
                const tuple = HIRValue{ .tuple = HIRTuple{
                    .elements = elements,
                } };
                try self.stack.push(HIRFrame.initFromHIRValue(tuple));
            },

            .TupleGet => |t| {
                const tuple = try self.stack.pop();

                switch (tuple.value) {
                    .tuple => |tup| {
                        if (t.index >= tup.elements.len) {
                            return ErrorList.IndexOutOfBounds;
                        }
                        try self.stack.push(HIRFrame.initFromHIRValue(tup.elements[t.index]));
                    },
                    else => return ErrorList.TypeError,
                }
            },

            .EnumNew => {
                // Enum support not implemented yet
                return ErrorList.NotImplemented;
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

                            // Use the field name or build path if available
                            if (value.field_name) |field_name| {
                                try std.io.getStdOut().writer().print("{s} = ", .{field_name});
                            } else {
                                try std.io.getStdOut().writer().print("{s} = ", .{i.field_names[0]});
                            }

                            try self.formatHIRValue(std.io.getStdOut().writer(), value.value);
                            try std.io.getStdOut().writer().print("\n", .{});
                        }
                    },
                }

                // Push the value back onto the stack for potential further use
                try self.stack.push(value);
            },

            .TypeOf => {
                // Type reflection not implemented yet
                return ErrorList.NotImplemented;
            },

            .Halt => {
                self.running = false;
            },

            .Map => |m| {
                const entries = try self.allocator.alloc(HIRMapEntry, m.entries.len);
                var i: usize = 0;
                while (i < m.entries.len) : (i += 1) {
                    const value = try self.stack.pop();
                    const key = try self.stack.pop();
                    entries[i] = HIRMapEntry{
                        .key = key.value,
                        .value = value.value,
                    };
                }
                const map = HIRValue{ .map = HIRMap{
                    .entries = entries,
                    .key_type = m.key_type,
                    .value_type = m.value_type,
                } };
                try self.stack.push(HIRFrame.initFromHIRValue(map));
            },

            .MapGet => {
                const key = try self.stack.pop();
                const map = try self.stack.pop();

                switch (map.value) {
                    .map => |m| {
                        for (m.entries) |entry| {
                            if (std.meta.eql(entry.key, key.value)) {
                                try self.stack.push(HIRFrame.initFromHIRValue(entry.value));
                                return;
                            }
                        }
                        try self.stack.push(HIRFrame.initNothing());
                    },
                    else => return ErrorList.TypeError,
                }
            },

            .Dup => {
                const value = self.stack.peek() orelse return self.reporter.reportError("Stack underflow in Dup", .{});
                var frame = HIRFrame.initFromHIRValue(value.value);
                frame.field_name = value.field_name;
                try self.stack.push(frame);
            },

            .Pop => {
                _ = try self.stack.pop();
            },
        }
    }

    fn compareEqual(self: *HIRVM, a: HIRFrame, b: HIRFrame) !bool {
        return switch (a.value) {
            .int => |i| switch (b.value) {
                .int => |j| i == j,
                else => false,
            },
            .float => |f| switch (b.value) {
                .float => |g| f == g,
                else => false,
            },
            .string => |s| switch (b.value) {
                .string => |t| std.mem.eql(u8, s, t),
                else => false,
            },
            .tetra => |t| switch (b.value) {
                .tetra => |u| t == u,
                else => false,
            },
            .struct_instance => |s| switch (b.value) {
                .struct_instance => |t| blk: {
                    if (!std.mem.eql(u8, s.type_name, t.type_name)) break :blk false;
                    if (s.fields.len != t.fields.len) break :blk false;
                    for (s.fields, t.fields) |field_a, field_b| {
                        if (!std.mem.eql(u8, field_a.name, field_b.name)) break :blk false;
                        const frame_a = HIRFrame.initFromHIRValue(field_a.value);
                        const frame_b = HIRFrame.initFromHIRValue(field_b.value);
                        if (!try self.compareEqual(frame_a, frame_b)) break :blk false;
                    }
                    break :blk true;
                },
                else => false,
            },
            .array => |arr| switch (b.value) {
                .array => |brr| blk: {
                    if (arr.elements.len != brr.elements.len) break :blk false;
                    for (arr.elements, brr.elements) |elem_a, elem_b| {
                        const frame_a = HIRFrame.initFromHIRValue(elem_a);
                        const frame_b = HIRFrame.initFromHIRValue(elem_b);
                        if (!try self.compareEqual(frame_a, frame_b)) break :blk false;
                    }
                    break :blk true;
                },
                else => false,
            },
            .tuple => |tup| switch (b.value) {
                .tuple => |tup_b| blk: {
                    if (tup.elements.len != tup_b.elements.len) break :blk false;
                    for (tup.elements, tup_b.elements) |elem_a, elem_b| {
                        const frame_a = HIRFrame.initFromHIRValue(elem_a);
                        const frame_b = HIRFrame.initFromHIRValue(elem_b);
                        if (!try self.compareEqual(frame_a, frame_b)) break :blk false;
                    }
                    break :blk true;
                },
                else => false,
            },
            .map => |map| switch (b.value) {
                .map => |map_b| blk: {
                    if (map.entries.len != map_b.entries.len) break :blk false;
                    for (map.entries) |entry_a| {
                        var found = false;
                        for (map_b.entries) |entry_b| {
                            const key_a = HIRFrame.initFromHIRValue(entry_a.key);
                            const key_b = HIRFrame.initFromHIRValue(entry_b.key);
                            if (try self.compareEqual(key_a, key_b)) {
                                const val_a = HIRFrame.initFromHIRValue(entry_a.value);
                                const val_b = HIRFrame.initFromHIRValue(entry_b.value);
                                if (!try self.compareEqual(val_a, val_b)) break :blk false;
                                found = true;
                                break;
                            }
                        }
                        if (!found) break :blk false;
                    }
                    break :blk true;
                },
                else => false,
            },
            .nothing => switch (b.value) {
                .nothing => true,
                else => false,
            },
            .u8 => |byte| switch (b.value) {
                .u8 => |other| byte == other,
                else => false,
            },
            .enum_variant => |enum_val| switch (b.value) {
                .enum_variant => |other| std.mem.eql(u8, enum_val.variant_name, other.variant_name),
                else => false,
            },
        };
    }

    fn compareLess(self: *HIRVM, a: HIRFrame, b: HIRFrame) !bool {
        // Keep self parameter for future error reporting and debugging
        _ = self;
        return switch (a.value) {
            .int => |i| switch (b.value) {
                .int => |j| i < j,
                else => false,
            },
            .float => |f| switch (b.value) {
                .float => |g| f < g,
                else => false,
            },
            .string => |s| switch (b.value) {
                .string => |t| std.mem.lessThan(u8, s, t),
                else => false,
            },
            else => false,
        };
    }

    fn compareGreater(self: *HIRVM, a: HIRFrame, b: HIRFrame) !bool {
        // Keep self parameter for future error reporting and debugging
        _ = self;
        return switch (a.value) {
            .int => |i| switch (b.value) {
                .int => |j| i > j,
                else => false,
            },
            .float => |f| switch (b.value) {
                .float => |g| f > g,
                else => false,
            },
            .string => |s| switch (b.value) {
                .string => |t| std.mem.lessThan(u8, t, s),
                else => false,
            },
            else => false,
        };
    }

    fn logicalAnd(self: *HIRVM, a: HIRFrame, b: HIRFrame) !u8 {
        // Keep self parameter for future error reporting and debugging
        _ = self;
        const a_tetra = try a.asTetra();
        const b_tetra = try b.asTetra();
        return switch (a_tetra) {
            0 => switch (b_tetra) { // false
                0 => 0, // false
                1 => 0, // false
                2 => 0, // false
                3 => 0, // false
                else => return ErrorList.TypeError,
            },
            1 => switch (b_tetra) { // true
                0 => 0, // false
                1 => 1, // true
                2 => 2, // both
                3 => 3, // neither
                else => return ErrorList.TypeError,
            },
            2 => switch (b_tetra) { // both
                0 => 0, // false
                1 => 2, // both
                2 => 2, // both
                3 => 3, // neither
                else => return ErrorList.TypeError,
            },
            3 => switch (b_tetra) { // neither
                0 => 0, // false
                1 => 3, // neither
                2 => 3, // neither
                3 => 3, // neither
                else => return ErrorList.TypeError,
            },
            else => return ErrorList.TypeError,
        };
    }

    fn logicalOr(self: *HIRVM, a: HIRFrame, b: HIRFrame) !u8 {
        // Keep self parameter for future error reporting and debugging
        _ = self;
        const a_tetra = try a.asTetra();
        const b_tetra = try b.asTetra();
        return switch (a_tetra) {
            0 => switch (b_tetra) { // false
                0 => 0, // false
                1 => 1, // true
                2 => 2, // both
                3 => 3, // neither
                else => return ErrorList.TypeError,
            },
            1 => switch (b_tetra) { // true
                0 => 1, // true
                1 => 1, // true
                2 => 2, // both
                3 => 1, // true
                else => return ErrorList.TypeError,
            },
            2 => switch (b_tetra) { // both
                0 => 2, // both
                1 => 2, // both
                2 => 2, // both
                3 => 2, // both
                else => return ErrorList.TypeError,
            },
            3 => switch (b_tetra) { // neither
                0 => 3, // neither
                1 => 1, // true
                2 => 2, // both
                3 => 3, // neither
                else => return ErrorList.TypeError,
            },
            else => return ErrorList.TypeError,
        };
    }

    fn logicalNot(self: *HIRVM, a: HIRFrame) !u8 {
        // Keep self parameter for future error reporting and debugging
        _ = self;
        const a_tetra = try a.asTetra();
        return switch (a_tetra) {
            0 => 1, // false -> true
            1 => 0, // true -> false
            2 => 2, // both -> both
            3 => 3, // neither -> neither
            else => return ErrorList.TypeError,
        };
    }

    fn logicalIff(self: *HIRVM, a: HIRFrame, b: HIRFrame) !u8 {
        // Keep self parameter for future error reporting and debugging
        _ = self;
        const a_tetra = try a.asTetra();
        const b_tetra = try b.asTetra();
        return switch (a_tetra) {
            0 => switch (b_tetra) { // false
                0 => 1, // true (false iff false)
                1 => 0, // false (false iff true)
                2 => 0, // false (false iff both)
                3 => 0, // false (false iff neither)
                else => return ErrorList.TypeError,
            },
            1 => switch (b_tetra) { // true
                0 => 0, // false (true iff false)
                1 => 1, // true (true iff true)
                2 => 0, // false (true iff both)
                3 => 0, // false (true iff neither)
                else => return ErrorList.TypeError,
            },
            2 => switch (b_tetra) { // both
                0 => 0, // false (both iff false)
                1 => 0, // false (both iff true)
                2 => 1, // true (both iff both)
                3 => 0, // false (both iff neither)
                else => return ErrorList.TypeError,
            },
            3 => switch (b_tetra) { // neither
                0 => 0, // false (neither iff false)
                1 => 0, // false (neither iff true)
                2 => 0, // false (neither iff both)
                3 => 1, // true (neither iff neither)
                else => return ErrorList.TypeError,
            },
            else => return ErrorList.TypeError,
        };
    }

    fn logicalXor(self: *HIRVM, a: HIRFrame, b: HIRFrame) !u8 {
        // Keep self parameter for future error reporting and debugging
        _ = self;
        const a_tetra = try a.asTetra();
        const b_tetra = try b.asTetra();
        return switch (a_tetra) {
            0 => switch (b_tetra) { // false
                0 => 0, // false (false xor false)
                1 => 1, // true (false xor true)
                2 => 2, // both (false xor both)
                3 => 3, // neither (false xor neither)
                else => return ErrorList.TypeError,
            },
            1 => switch (b_tetra) { // true
                0 => 1, // true (true xor false)
                1 => 0, // false (true xor true)
                2 => 2, // both (true xor both)
                3 => 3, // neither (true xor neither)
                else => return ErrorList.TypeError,
            },
            2 => switch (b_tetra) { // both
                0 => 2, // both (both xor false)
                1 => 2, // both (both xor true)
                2 => 0, // false (both xor both)
                3 => 2, // both (both xor neither)
                else => return ErrorList.TypeError,
            },
            3 => switch (b_tetra) { // neither
                0 => 3, // neither (neither xor false)
                1 => 3, // neither (neither xor true)
                2 => 2, // both (neither xor both)
                3 => 0, // false (neither xor neither)
                else => return ErrorList.TypeError,
            },
            else => return ErrorList.TypeError,
        };
    }

    fn logicalNand(self: *HIRVM, a: HIRFrame, b: HIRFrame) !u8 {
        // NAND is equivalent to NOT(AND)
        const and_result = try self.logicalAnd(a, b);
        return switch (and_result) {
            0 => 1, // false -> true
            1 => 0, // true -> false
            2 => 2, // both -> both
            3 => 3, // neither -> neither
            else => return ErrorList.TypeError,
        };
    }

    fn logicalNor(self: *HIRVM, a: HIRFrame, b: HIRFrame) !u8 {
        // NOR is equivalent to NOT(OR)
        const or_result = try self.logicalOr(a, b);
        return switch (or_result) {
            0 => 1, // false -> true
            1 => 0, // true -> false
            2 => 2, // both -> both
            3 => 3, // neither -> neither
            else => return ErrorList.TypeError,
        };
    }

    fn logicalImplies(self: *HIRVM, a: HIRFrame, b: HIRFrame) !u8 {
        // Keep self parameter for future error reporting and debugging
        _ = self;
        const a_tetra = try a.asTetra();
        const b_tetra = try b.asTetra();
        return switch (a_tetra) {
            0 => 1, // false implies anything is true
            1 => b_tetra, // true implies b
            2 => switch (b_tetra) { // both implies b
                0 => 0, // false
                1 => 1, // true
                2 => 2, // both
                3 => 3, // neither
                else => return ErrorList.TypeError,
            },
            3 => switch (b_tetra) { // neither implies b
                0 => 3, // neither
                1 => 1, // true
                2 => 2, // both
                3 => 3, // neither
                else => return ErrorList.TypeError,
            },
            else => return ErrorList.TypeError,
        };
    }

    fn stringConcat(self: *HIRVM, a: HIRFrame, b: HIRFrame) !HIRFrame {
        const a_str = try a.asString();
        const b_str = try b.asString();
        const result = try std.fmt.allocPrint(self.allocator, "{s}{s}", .{ a_str, b_str });
        return HIRFrame.initString(result);
    }

    fn stringLength(self: *HIRVM, a: HIRFrame) !HIRFrame {
        _ = self; // Not needed for string length calculation
        const str = try a.asString();
        return HIRFrame.initInt(@as(i32, @intCast(str.len)));
    }

    fn stringSubstring(self: *HIRVM, str: HIRFrame, start: HIRFrame, len: HIRFrame) !HIRFrame {
        const s = try str.asString();
        const start_idx = try start.asInt();
        const length = try len.asInt();

        if (start_idx < 0 or length < 0) {
            return ErrorList.IndexOutOfBounds;
        }

        const start_usize = @as(usize, @intCast(start_idx));
        const len_usize = @as(usize, @intCast(length));

        if (start_usize + len_usize > s.len) {
            return ErrorList.IndexOutOfBounds;
        }

        const result = try self.allocator.dupe(u8, s[start_usize .. start_usize + len_usize]);
        return HIRFrame.initString(result);
    }
};
