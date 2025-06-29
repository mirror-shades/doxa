const std = @import("std");
const hir = @import("../codegen/soxa.zig");
const HIRInstruction = hir.HIRInstruction;
const HIRValue = hir.HIRValue;
const HIRArray = hir.HIRArray;
const HIRProgram = hir.HIRProgram;
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

const STACK_SIZE: u32 = 1024 * 1024;

/// Hot variable cache entry for ultra-fast variable lookup
const HotVar = struct {
    name: []const u8,
    storage_id: u32,
};

/// HIR-based VM Frame - simplified to work with memory management
pub const HIRFrame = struct {
    value: HIRValue,

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

    pub fn initBoolean(x: bool) HIRFrame {
        return HIRFrame{ .value = HIRValue{ .boolean = x } };
    }

    pub fn initU8(x: u8) HIRFrame {
        return HIRFrame{ .value = HIRValue{ .u8 = x } };
    }

    pub fn initNothing() HIRFrame {
        return HIRFrame{ .value = HIRValue.nothing };
    }

    pub fn initFromHIRValue(value: HIRValue) HIRFrame {
        return HIRFrame{ .value = value };
    }

    pub fn asInt(self: HIRFrame) !i32 {
        return switch (self.value) {
            .int => |i| i,
            else => error.TypeError,
        };
    }

    pub fn asFloat(self: HIRFrame) !f64 {
        return switch (self.value) {
            .float => |f| f,
            else => error.TypeError,
        };
    }

    pub fn asBoolean(self: HIRFrame) !bool {
        return switch (self.value) {
            .boolean => |b| b,
            else => error.TypeError,
        };
    }

    pub fn asString(self: HIRFrame) ![]const u8 {
        return switch (self.value) {
            .string => |s| s,
            else => error.TypeError,
        };
    }

    pub fn asU8(self: HIRFrame) !u8 {
        return switch (self.value) {
            .u8 => |u| u,
            else => error.TypeError,
        };
    }
};

/// Utility functions to convert between HIR and memory system types
pub fn hirValueToTokenLiteral(hir_value: HIRValue) TokenLiteral {
    return switch (hir_value) {
        .int => |i| TokenLiteral{ .int = i },
        .u8 => |u| TokenLiteral{ .u8 = u },
        .float => |f| TokenLiteral{ .float = f },
        .string => |s| TokenLiteral{ .string = s },
        .boolean => |b| TokenLiteral{ .tetra = if (b) .true else .false },
        .nothing => TokenLiteral{ .nothing = {} },
        // Convert HIR arrays to TokenLiteral arrays
        .array => |arr| blk: {
            // Create a TokenLiteral array from HIR array
            var token_elements = std.ArrayList(TokenLiteral).init(std.heap.page_allocator);
            defer token_elements.deinit();

            for (arr.elements) |elem| {
                if (std.meta.eql(elem, HIRValue.nothing)) break;
                token_elements.append(hirValueToTokenLiteral(elem)) catch break;
            }

            break :blk TokenLiteral{ .array = token_elements.toOwnedSlice() catch &[_]TokenLiteral{} };
        },
        // Other complex types still convert to nothing
        .struct_instance => TokenLiteral{ .nothing = {} },
        .tuple => TokenLiteral{ .nothing = {} },
        .map => TokenLiteral{ .nothing = {} },
        .enum_variant => TokenLiteral{ .nothing = {} },
    };
}

pub fn tokenLiteralToHIRValue(token_literal: TokenLiteral) HIRValue {
    return switch (token_literal) {
        .int => |i| HIRValue{ .int = i },
        .u8 => |u| HIRValue{ .u8 = u },
        .float => |f| HIRValue{ .float = f },
        .string => |s| HIRValue{ .string = s },
        .tetra => |t| HIRValue{ .boolean = t == .true },
        .nothing => HIRValue.nothing,
        // Convert TokenLiteral arrays back to HIR arrays
        .array => |arr| blk: {
            // Allocate mutable array directly
            const elements = std.heap.page_allocator.alloc(HIRValue, arr.len) catch {
                // On allocation failure, return empty array
                break :blk HIRValue{
                    .array = HIRArray{
                        .elements = &[_]HIRValue{},
                        .element_type = .Auto,
                        .capacity = 0,
                    },
                };
            };

            for (arr, 0..) |elem, i| {
                if (i >= elements.len) break;
                elements[i] = tokenLiteralToHIRValue(elem);
            }

            break :blk HIRValue{
                .array = HIRArray{
                    .elements = elements,
                    .element_type = .Auto, // Infer from first element
                    .capacity = @intCast(elements.len),
                },
            };
        },
        // Other complex types still convert to nothing
        .tuple => HIRValue.nothing,
        .struct_value => HIRValue.nothing,
        .function => HIRValue.nothing,
        .enum_variant => HIRValue.nothing,
        .map => HIRValue.nothing,
    };
}

pub fn hirValueToTokenType(hir_value: HIRValue) TokenType {
    return switch (hir_value) {
        .int => .INT,
        .u8 => .U8,
        .float => .FLOAT,
        .string => .STRING,
        .boolean => .TETRA,
        .nothing => .NOTHING,
        // Complex types - map to closest TokenType
        .array => .ARRAY,
        .struct_instance => .IDENTIFIER, // Structs represented as identifiers
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
        .boolean => TypeInfo{ .base = .Tetra, .is_mutable = true },
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

    pub fn push(self: *HIRStack, value: HIRFrame) !void {
        if (self.sp >= STACK_SIZE) {
            std.debug.print("Stack overflow: Attempted to push at sp={}\n", .{self.sp});
            return error.StackOverflow;
        }
        self.data[@intCast(self.sp)] = value;
        self.sp += 1;

        // Debug output is now handled by the VM's debug mode
    }

    pub fn pop(self: *HIRStack) !HIRFrame {
        if (self.sp <= 0) {
            std.debug.print("Stack underflow: Attempted to pop at sp={}\n", .{self.sp});
            return error.StackUnderflow;
        }
        self.sp -= 1;
        const value = self.data[@intCast(self.sp)];
        return value;
    }

    pub fn peek(self: HIRStack) !HIRFrame {
        if (self.sp <= 0) {
            std.debug.print("Stack underflow: Attempted to peek at sp={}\n", .{self.sp});
            return error.StackUnderflow;
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

/// HIR-based Virtual Machine - executes HIR instructions directly
pub const HIRVM = struct {
    program: *HIRProgram,
    reporter: *Reporter,
    memory_manager: *MemoryManager, // Use existing memory manager from main
    allocator: std.mem.Allocator,

    // Execution state
    ip: u32 = 0, // Instruction pointer (index into instructions array)
    stack: HIRStack,
    current_scope: *Scope, // Current execution scope
    running: bool = true,

    // Label resolution
    label_map: std.StringHashMap(u32), // label_name -> instruction index

    // Performance optimizations
    var_cache: std.StringHashMap(u32), // Cache variable name → storage_id mapping
    fast_mode: bool = true, // Enable optimizations for computational workloads
    turbo_mode: bool = true, // Ultra-aggressive optimizations for pure computational loops

    // Hot variable cache - direct storage for most accessed variables
    hot_vars: [4]?HotVar = [_]?HotVar{null} ** 4,
    hot_var_count: u8 = 0,

    pub fn init(allocator: std.mem.Allocator, program: *HIRProgram, reporter: *Reporter, memory_manager: *MemoryManager) !HIRVM {
        // Create a new execution scope for this HIR VM run
        // This allows proper cleanup and isolation
        const execution_scope = try memory_manager.scope_manager.createScope(memory_manager.scope_manager.root_scope);

        var vm = HIRVM{
            .program = program,
            .reporter = reporter,
            .memory_manager = memory_manager,
            .allocator = allocator,
            .stack = try HIRStack.init(allocator),
            .current_scope = execution_scope,
            .label_map = std.StringHashMap(u32).init(allocator),
            .var_cache = std.StringHashMap(u32).init(allocator),
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
        self.stack.deinit();
        self.label_map.deinit();
        self.var_cache.deinit();

        // Clean up the execution scope (this will clean up all variables created during execution)
        self.current_scope.deinit();
    }

    /// Pre-resolve all labels to instruction indices for O(1) jump lookup
    fn resolveLabels(self: *HIRVM) !void {
        for (self.program.instructions, 0..) |instruction, i| {
            switch (instruction) {
                .Label => |label| {
                    try self.label_map.put(label.name, @intCast(i));
                    if (self.memory_manager.debug_enabled) {
                        std.debug.print("Resolved label '{s}' to instruction {}\n", .{ label.name, i });
                    }
                },
                else => {},
            }
        }
    }

    /// Main execution loop - directly execute HIR instructions
    pub fn run(self: *HIRVM) !?HIRFrame {
        if (self.memory_manager.debug_enabled) {
            std.debug.print(">> Starting HIR VM execution with {} instructions\n", .{self.program.instructions.len});
        }

        while (self.running and self.ip < self.program.instructions.len) {
            const instruction = self.program.instructions[self.ip];

            // OPTIMIZED: Reduce debug output in fast mode for better performance
            if (self.memory_manager.debug_enabled and (!self.fast_mode or self.ip < 10)) {
                std.debug.print("HIR VM [{}]: Executing {s}\n", .{ self.ip, @tagName(instruction) });
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

    /// Execute a single HIR instruction
    fn executeInstruction(self: *HIRVM, instruction: HIRInstruction) !void {
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
                                const hir_value = tokenLiteralToHIRValue(storage.value);
                                try self.stack.push(HIRFrame.initFromHIRValue(hir_value));
                                return; // Ultra-fast path
                            }
                            break;
                        }
                    }
                }

                if (self.fast_mode) {
                    // OPTIMIZED: Use cached storage_id for O(1) variable access
                    if (self.var_cache.get(v.var_name)) |storage_id| {
                        if (self.memory_manager.scope_manager.value_storage.get(storage_id)) |storage| {
                            // Add to hot cache if frequently accessed
                            if (self.turbo_mode and self.hot_var_count < 4) {
                                const found_in_hot = for (self.hot_vars[0..self.hot_var_count]) |hot_var| {
                                    if (hot_var != null and std.mem.eql(u8, hot_var.?.name, v.var_name)) break true;
                                } else false;

                                if (!found_in_hot) {
                                    self.hot_vars[self.hot_var_count] = HotVar{ .name = v.var_name, .storage_id = storage_id };
                                    self.hot_var_count += 1;
                                }
                            }

                            const hir_value = tokenLiteralToHIRValue(storage.value);
                            try self.stack.push(HIRFrame.initFromHIRValue(hir_value));
                            return; // Fast path - skip slow lookup
                        }
                    }
                }

                // FALLBACK: Standard variable lookup (populate cache for next time)
                if (self.current_scope.lookupVariable(v.var_name)) |variable| {
                    if (self.memory_manager.scope_manager.value_storage.get(variable.storage_id)) |storage| {
                        // Cache this lookup for future O(1) access
                        if (self.fast_mode) {
                            self.var_cache.put(v.var_name, variable.storage_id) catch {};
                        }

                        const hir_value = tokenLiteralToHIRValue(storage.value);
                        try self.stack.push(HIRFrame.initFromHIRValue(hir_value));
                    } else {
                        return self.reporter.reportError("Variable storage not found for: {s}", .{v.var_name});
                    }
                } else {
                    return self.reporter.reportError("Undefined variable: {s}", .{v.var_name});
                }
            },

            .StoreVar => |v| {
                // Store top of stack to variable
                const value = try self.stack.pop();
                const token_literal = hirValueToTokenLiteral(value.value);
                const token_type = hirValueToTokenType(value.value);
                const type_info = hirValueToTypeInfo(value.value);

                if (self.fast_mode) {
                    // OPTIMIZED: Use cached storage_id for O(1) variable access
                    if (self.var_cache.get(v.var_name)) |storage_id| {
                        if (self.memory_manager.scope_manager.value_storage.getPtr(storage_id)) |storage| {
                            if (storage.*.constant) {
                                return self.reporter.reportError("Cannot modify constant variable: {s}", .{v.var_name});
                            }
                            storage.*.value = token_literal;
                            storage.*.type = token_type;
                            storage.*.type_info = type_info;
                            return; // Fast path - skip slow lookup
                        }
                    }
                }

                // FALLBACK: Standard variable lookup/creation
                if (self.current_scope.lookupVariable(v.var_name)) |variable| {
                    // Update existing variable's storage
                    if (self.memory_manager.scope_manager.value_storage.getPtr(variable.storage_id)) |storage| {
                        if (storage.*.constant) {
                            return self.reporter.reportError("Cannot modify constant variable: {s}", .{v.var_name});
                        }

                        // Cache this lookup for future O(1) access
                        if (self.fast_mode) {
                            self.var_cache.put(v.var_name, variable.storage_id) catch {};
                        }

                        storage.*.value = token_literal;
                        storage.*.type = token_type;
                        storage.*.type_info = type_info;
                    } else {
                        return self.reporter.reportError("Variable storage not found for: {s}", .{v.var_name});
                    }
                } else {
                    // Create new variable in current scope
                    _ = self.current_scope.createValueBinding(v.var_name, token_literal, token_type, type_info, false) catch |err| {
                        return self.reporter.reportError("Failed to create variable {s}: {}", .{ v.var_name, err });
                    };
                }
            },

            .IntArith => |a| {
                const b = try self.stack.pop();
                const a_val = try self.stack.pop();

                const result = switch (a.op) {
                    .Add => try self.intAdd(try a_val.asInt(), try b.asInt()),
                    .Sub => try self.intSub(try a_val.asInt(), try b.asInt()),
                    .Mul => try self.intMul(try a_val.asInt(), try b.asInt()),
                    .Div => try self.intDiv(try a_val.asInt(), try b.asInt()),
                    .Mod => try self.intMod(try a_val.asInt(), try b.asInt()),
                };

                try self.stack.push(HIRFrame.initInt(result));
            },

            .FloatArith => |a| {
                const b = try self.stack.pop();
                const a_val = try self.stack.pop();

                const result = switch (a.op) {
                    .Add => try self.floatAdd(try a_val.asFloat(), try b.asFloat()),
                    .Sub => try self.floatSub(try a_val.asFloat(), try b.asFloat()),
                    .Mul => try self.floatMul(try a_val.asFloat(), try b.asFloat()),
                    .Div => try self.floatDiv(try a_val.asFloat(), try b.asFloat()),
                    .Mod => return error.UnsupportedOperation, // Float modulo not supported
                };

                try self.stack.push(HIRFrame.initFloat(result));
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

                try self.stack.push(HIRFrame.initBoolean(result));
            },

            .Jump => |j| {
                // Unconditional jump to label
                if (self.label_map.get(j.label)) |target_ip| {
                    self.ip = target_ip;
                    if (self.memory_manager.debug_enabled and !self.fast_mode) {
                        std.debug.print("Jumping to label '{s}' at instruction {}\n", .{ j.label, target_ip });
                    }
                } else {
                    return self.reporter.reportFatalError("Unknown label: {s}", .{j.label});
                }
            },

            .JumpCond => |j| {
                // OPTIMIZED: Conditional jump with reduced overhead
                const condition = try self.stack.pop();
                const should_jump = switch (condition.value) {
                    .boolean => |b| b,
                    .int => |i| i != 0,
                    .float => |f| f != 0.0,
                    .nothing => false,
                    else => true,
                };

                const target_label = if (should_jump) j.label_true else j.label_false;
                if (self.label_map.get(target_label)) |target_ip| {
                    self.ip = target_ip;
                    if (self.memory_manager.debug_enabled and !self.fast_mode) {
                        std.debug.print("Conditional jump to '{s}' at instruction {}\n", .{ target_label, target_ip });
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

            .Inspect => |i| {
                // OPTIMIZED: Use proper UTF-8 buffered output like the old interpreter
                const value = try self.stack.peek();

                var buffer = std.ArrayList(u8).init(self.allocator);
                defer buffer.deinit();

                // Format like the old interpreter - proper UTF-8 handling
                if (i.name) |name| {
                    try buffer.writer().print("INSPECT {s}: ", .{name});
                } else {
                    try buffer.writer().print("INSPECT: ", .{});
                }

                // Format the value using the same approach as old interpreter
                try self.formatHIRValue(buffer.writer(), value.value);
                try buffer.writer().print("\n", .{});

                // Use std.io.getStdOut().writeAll for proper UTF-8 output (like old interpreter)
                try std.io.getStdOut().writeAll(buffer.items);
            },

            .EnterScope => |s| {
                if (self.fast_mode) {
                    // OPTIMIZED: Skip scope creation for simple blocks (major optimization)
                    // Only create scopes when absolutely necessary (when new variables are declared)
                    if (self.memory_manager.debug_enabled) {
                        std.debug.print("[FAST] Skipping scope creation {} for performance\n", .{s.scope_id});
                    }
                } else {
                    // Standard scope creation
                    const new_scope = try self.memory_manager.scope_manager.createScope(self.current_scope);
                    self.current_scope = new_scope;
                    if (self.memory_manager.debug_enabled) {
                        std.debug.print("Entered scope {} (parent: {})\n", .{ s.scope_id, new_scope.parent.?.id });
                    }
                }
            },

            .ExitScope => |s| {
                if (self.fast_mode) {
                    // OPTIMIZED: Skip scope cleanup for simple blocks
                    if (self.memory_manager.debug_enabled) {
                        std.debug.print("[FAST] Skipping scope cleanup {} for performance\n", .{s.scope_id});
                    }
                } else {
                    // Standard scope cleanup
                    if (self.current_scope.parent) |parent_scope| {
                        const old_scope = self.current_scope;
                        self.current_scope = parent_scope;
                        if (self.memory_manager.debug_enabled) {
                            std.debug.print("Exited scope {} (returned to: {})\n", .{ s.scope_id, parent_scope.id });
                        }
                        // Clean up the old scope
                        old_scope.deinit();
                    } else {
                        return self.reporter.reportError("Cannot exit root scope", .{});
                    }
                }
            },

            .Halt => {
                if (self.memory_manager.debug_enabled) {
                    std.debug.print("HIR VM halted\n", .{});
                }
                self.running = false;
            },

            // Logical operations (from old VM - proven implementations)
            .LogicalOp => |l| {
                const b = try self.stack.pop();
                const a = try self.stack.pop();

                const result = switch (l.op) {
                    .And => try self.logicalAnd(a, b),
                    .Or => try self.logicalOr(a, b),
                    .Not => blk: {
                        // For NOT, we only use 'a', push 'b' back
                        try self.stack.push(b);
                        break :blk try self.logicalNot(a);
                    },
                };

                try self.stack.push(HIRFrame.initBoolean(result));
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
                const elements = try self.allocator.alloc(HIRValue, a.size);
                // Initialize all elements to nothing
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

            .ArrayGet => |a| {
                // Get array element by index: array[index]
                const index = try self.stack.pop(); // Index
                const array = try self.stack.pop(); // Array

                const index_val = switch (index.value) {
                    .int => |i| if (i < 0) {
                        return self.reporter.reportError("Array index cannot be negative: {}", .{i});
                    } else @as(u32, @intCast(i)),
                    .u8 => |u| @as(u32, u),
                    else => return self.reporter.reportError("Array index must be an integer, got: {s}", .{@tagName(index.value)}),
                };

                switch (array.value) {
                    .array => |arr| {
                        if (a.bounds_check and index_val >= arr.elements.len) {
                            return self.reporter.reportError("Array index out of bounds: {} >= {}", .{ index_val, arr.elements.len });
                        }

                        const element = arr.elements[index_val];
                        try self.stack.push(HIRFrame.initFromHIRValue(element));
                    },
                    else => return self.reporter.reportError("Cannot index non-array value: {s}", .{@tagName(array.value)}),
                }
            },

            .ArraySet => |a| {
                // Set array element by index
                // Stack order (top to bottom): value, index, array
                const value = try self.stack.pop(); // Value to set
                const index = try self.stack.pop(); // Index
                const array_frame = try self.stack.pop(); // Array

                const index_val = switch (index.value) {
                    .int => |i| if (i < 0) {
                        return self.reporter.reportError("Array index cannot be negative: {}", .{i});
                    } else @as(u32, @intCast(i)),
                    .u8 => |u| @as(u32, u),
                    else => return self.reporter.reportError("Array index must be an integer, got: {s}", .{@tagName(index.value)}),
                };

                switch (array_frame.value) {
                    .array => |arr| {
                        // Create a mutable copy of the array
                        var mutable_arr = arr;

                        if (a.bounds_check and index_val >= mutable_arr.elements.len) {
                            return self.reporter.reportError("Array index out of bounds: {} >= {}", .{ index_val, mutable_arr.elements.len });
                        }

                        mutable_arr.elements[index_val] = value.value;
                        // Push the modified array back onto the stack
                        const modified_array_value = HIRValue{ .array = mutable_arr };
                        try self.stack.push(HIRFrame.initFromHIRValue(modified_array_value));
                    },
                    else => return self.reporter.reportError("Cannot index non-array value: {s}", .{@tagName(array_frame.value)}),
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
                    else => return self.reporter.reportError("Cannot push to non-array value: {s}", .{@tagName(array.value)}),
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

            .Call => |c| {
                // Handle function calls
                if (self.memory_manager.debug_enabled) {
                    std.debug.print("Function call: {s} with {} args (kind: {s})\n", .{ c.qualified_name, c.arg_count, @tagName(c.call_kind) });
                }

                switch (c.call_kind) {
                    .LocalFunction => {
                        // User-defined function call with proper stack management
                        if (c.function_index >= self.program.function_table.len) {
                            return self.reporter.reportError("Invalid function index: {} (max: {})", .{ c.function_index, self.program.function_table.len });
                        }

                        const function = self.program.function_table[c.function_index];
                        if (self.memory_manager.debug_enabled) {
                            std.debug.print("Calling user function: {s} with {} args at label: {s}\n", .{ function.name, c.arg_count, function.start_label });
                        }

                        // Arguments are already on the stack in the correct order for parameter setup
                        // The function's parameter setup (StoreVar instructions) will handle them
                        // We just need to jump to the function start
                        if (self.memory_manager.debug_enabled) {
                            std.debug.print(">> Stack has {} args ready for parameter setup\n", .{c.arg_count});
                        }

                        // Find the function label instruction and jump to it
                        for (self.program.instructions, 0..) |instr, i| {
                            if (instr == .Label and std.mem.eql(u8, instr.Label.name, function.start_label)) {
                                self.ip = @intCast(i);
                                return; // Jump to function start
                            }
                        }

                        return self.reporter.reportError("Function label not found: {s}", .{function.start_label});
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
                                        // For now, just return an error if full
                                        return self.reporter.reportError("Array is full - cannot push more elements", .{});
                                    }

                                    // Add element to end
                                    mutable_arr.elements[length] = element.value;

                                    // Push the modified array back onto the stack
                                    const modified_array_value = HIRValue{ .array = mutable_arr };
                                    try self.stack.push(HIRFrame.initFromHIRValue(modified_array_value));
                                },
                                else => return self.reporter.reportError("Cannot push to non-array value: {s}", .{@tagName(array.value)}),
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
                if (self.memory_manager.debug_enabled) {
                    std.debug.print(">> Function return (has_value: {})\n", .{ret.has_value});
                }

                if (ret.has_value) {
                    // Return value is already on top of stack
                    // For now, just halt execution (simple implementation)
                    self.running = false;
                } else {
                    // No return value - void function, push nothing
                    try self.stack.push(HIRFrame.initFromHIRValue(HIRValue.nothing));
                    self.running = false;
                }
            },

            else => {
                if (self.memory_manager.debug_enabled) {
                    std.debug.print("!! Unhandled HIR instruction: {s}\n", .{@tagName(instruction)});
                }
                return error.UnhandledInstruction;
            },
        }
    }

    /// Check if the last instruction caused a jump (to avoid auto-incrementing IP)
    fn didJump(self: *HIRVM, instruction: HIRInstruction) bool {
        _ = self;
        return switch (instruction) {
            .Jump, .JumpCond => true,
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
            return error.DivisionByZero;
        }
        return @divTrunc(a, b);
    }

    fn intMod(self: *HIRVM, a: i32, b: i32) !i32 {
        _ = self;
        if (b == 0) {
            std.debug.print("Modulo by zero: {} % {}\n", .{ a, b });
            return error.DivisionByZero;
        }
        return @mod(a, b);
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
            return error.DivisionByZero;
        }
        return a / b;
    }

    // ===============================================================================
    // LOGICAL OPERATIONS (From old VM - proven implementations)
    // ===============================================================================

    /// Logical AND operation - strict boolean typing from old VM
    fn logicalAnd(self: *HIRVM, a: HIRFrame, b: HIRFrame) !bool {
        const a_bool = switch (a.value) {
            .boolean => |val| val,
            else => {
                self.reporter.reportError("Cannot perform AND on non-boolean values: {s} AND {s}", .{ @tagName(a.value), @tagName(b.value) });
                return error.TypeError;
            },
        };

        const b_bool = switch (b.value) {
            .boolean => |val| val,
            else => {
                self.reporter.reportError("Cannot perform AND on non-boolean values: {s} AND {s}", .{ @tagName(a.value), @tagName(b.value) });
                return error.TypeError;
            },
        };

        return a_bool and b_bool;
    }

    /// Logical OR operation - strict boolean typing from old VM
    fn logicalOr(self: *HIRVM, a: HIRFrame, b: HIRFrame) !bool {
        const a_bool = switch (a.value) {
            .boolean => |val| val,
            else => {
                self.reporter.reportError("Cannot perform OR on non-boolean values: {s} OR {s}", .{ @tagName(a.value), @tagName(b.value) });
                return error.TypeError;
            },
        };

        const b_bool = switch (b.value) {
            .boolean => |val| val,
            else => {
                self.reporter.reportError("Cannot perform OR on non-boolean values: {s} OR {s}", .{ @tagName(a.value), @tagName(b.value) });
                return error.TypeError;
            },
        };

        return a_bool or b_bool;
    }

    /// Logical NOT operation - strict boolean typing from old VM
    fn logicalNot(self: *HIRVM, a: HIRFrame) !bool {
        const a_bool = switch (a.value) {
            .boolean => |val| val,
            else => {
                self.reporter.reportError("Cannot perform NOT on a non-boolean value: {s}", .{@tagName(a.value)});
                return error.TypeError;
            },
        };

        return !a_bool;
    }

    // ===============================================================================
    // STRING OPERATIONS (From old VM - proven implementations)
    // ===============================================================================

    /// String concatenation - creates new string from two input strings
    fn stringConcat(self: *HIRVM, a: HIRFrame, b: HIRFrame) !HIRFrame {
        const a_str = switch (a.value) {
            .string => |s| s,
            else => return error.TypeError,
        };

        const b_str = switch (b.value) {
            .string => |s| s,
            else => return error.TypeError,
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
            else => return error.TypeError,
        };

        return HIRFrame.initInt(@as(i32, @intCast(str.len)));
    }

    /// String substring operation - from old VM implementation
    fn stringSubstring(self: *HIRVM, str_frame: HIRFrame, start_frame: HIRFrame, len_frame: HIRFrame) !HIRFrame {
        const str = switch (str_frame.value) {
            .string => |s| s,
            else => return error.TypeError,
        };

        const start = switch (start_frame.value) {
            .int => |i| i,
            else => return error.TypeError,
        };

        const length = switch (len_frame.value) {
            .int => |i| i,
            else => return error.TypeError,
        };

        if (start < 0 or length < 0 or start >= str.len or start + length > str.len) {
            return error.IndexOutOfBounds;
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
            .boolean => |a_val| switch (b.value) {
                .boolean => |b_val| a_val == b_val,
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
                else => error.TypeError,
            },
            .float => |a_val| switch (b.value) {
                .float => |b_val| a_val < b_val,
                // Mixed float/int comparison (from old VM)
                .int => |b_val| a_val < @as(f64, @floatFromInt(b_val)),
                else => error.TypeError,
            },
            .u8 => |a_val| switch (b.value) {
                .u8 => |b_val| a_val < b_val,
                else => error.TypeError,
            },
            // Complex types don't support comparison
            .boolean, .string, .nothing, .array, .struct_instance, .tuple, .map, .enum_variant => error.TypeError,
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
                else => error.TypeError,
            },
            .float => |a_val| switch (b.value) {
                .float => |b_val| a_val > b_val,
                // Mixed float/int comparison (from old VM)
                .int => |b_val| a_val > @as(f64, @floatFromInt(b_val)),
                else => error.TypeError,
            },
            .u8 => |a_val| switch (b.value) {
                .u8 => |b_val| a_val > b_val,
                else => error.TypeError,
            },
            // Complex types don't support comparison
            .boolean, .string, .nothing, .array, .struct_instance, .tuple, .map, .enum_variant => error.TypeError,
        };
    }

    /// Print a HIR value for debugging/inspection
    pub fn printHIRValue(self: *HIRVM, value: HIRValue) !void {
        switch (value) {
            .int => |i| std.debug.print("{}", .{i}),
            .float => |f| std.debug.print("{d}", .{f}),
            .string => |s| std.debug.print("\"{s}\"", .{s}),
            .boolean => |b| std.debug.print("{}", .{b}),
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
            .boolean => |b| try writer.print("{}", .{b}),
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
        if (!self.memory_manager.debug_enabled) return;

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
