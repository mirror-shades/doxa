const std = @import("std");
const HIRInstruction = @import("soxa_instructions.zig").HIRInstruction;
const HIRValue = @import("soxa_values.zig").HIRValue;
const ArithOp = @import("soxa_instructions.zig").ArithOp;
const Reporter = @import("../../utils/reporting.zig").Reporter;

/// Comprehensive peephole optimizer for HIR instructions
/// Organized by optimization categories for maintainability
pub const PeepholeOptimizer = struct {
    allocator: std.mem.Allocator,
    reporter: *Reporter,

    // Optimization counters by category
    redundant_eliminations: u32 = 0,
    arithmetic_optimizations: u32 = 0,
    stack_optimizations: u32 = 0,
    variable_optimizations: u32 = 0,
    control_flow_optimizations: u32 = 0,

    pub fn init(allocator: std.mem.Allocator, reporter: *Reporter) PeepholeOptimizer {
        return PeepholeOptimizer{
            .allocator = allocator,
            .reporter = reporter,
        };
    }

    /// Apply comprehensive peephole optimizations
    pub fn optimize(self: *PeepholeOptimizer, instructions: []HIRInstruction) ![]HIRInstruction {
        var optimized = std.ArrayList(HIRInstruction).init(self.allocator);
        defer optimized.deinit();

        var i: usize = 0;
        while (i < instructions.len) {
            const consumed = try self.tryOptimizeAt(instructions, i, &optimized);
            i += consumed;
        }

        return optimized.toOwnedSlice();
    }

    /// Try to optimize at position i, returns number of instructions consumed
    fn tryOptimizeAt(self: *PeepholeOptimizer, instructions: []HIRInstruction, i: usize, optimized: *std.ArrayList(HIRInstruction)) !usize {
        // Try optimization patterns in order of complexity (most specific first)

        // CATEGORY 1: REDUNDANT INSTRUCTION ELIMINATION
        if (try self.tryRedundantElimination(instructions, i, optimized)) |consumed| {
            return consumed;
        }

        // CATEGORY 2: ARITHMETIC OPTIMIZATIONS
        if (try self.tryArithmeticOptimization(instructions, i, optimized)) |consumed| {
            return consumed;
        }

        // CATEGORY 3: VARIABLE ACCESS OPTIMIZATIONS
        if (try self.tryVariableOptimization(instructions, i, optimized)) |consumed| {
            return consumed;
        }

        // No optimization applied - pass through single instruction
        try optimized.append(instructions[i]);
        return 1;
    }

    //==================================================================
    // CATEGORY 1: REDUNDANT INSTRUCTION ELIMINATION
    //==================================================================

    fn tryRedundantElimination(self: *PeepholeOptimizer, instructions: []HIRInstruction, i: usize, optimized: *std.ArrayList(HIRInstruction)) !?usize {

        // Pattern 1: Detect chain of Dups followed by Peek + Pop → just Peek
        if (std.meta.activeTag(instructions[i]) == .Dup) {
            var dup_count: usize = 0;
            var j = i;

            // Count consecutive Dups
            while (j < instructions.len and std.meta.activeTag(instructions[j]) == .Dup) {
                dup_count += 1;
                j += 1;
            }

            // Check if followed by Peek + Pop
            if (j + 1 < instructions.len and
                std.meta.activeTag(instructions[j]) == .Peek and
                std.meta.activeTag(instructions[j + 1]) == .Pop)
            {
                // Replace entire chain with just Peek - MAXIMUM EFFICIENCY!
                try optimized.append(instructions[j]); // Just the Peek
                self.redundant_eliminations += @intCast(dup_count); // Count all eliminated Dups + Pop
                return dup_count + 2; // Consumed: Dups + Peek + Pop
            }
        }

        // Pattern 2: Single Dup + Peek + Pop → just Peek
        if (i + 2 < instructions.len and
            std.meta.activeTag(instructions[i]) == .Dup and
            std.meta.activeTag(instructions[i + 1]) == .Peek and
            std.meta.activeTag(instructions[i + 2]) == .Pop)
        {
            try optimized.append(instructions[i + 1]); // Just Peek
            self.redundant_eliminations += 1;
            return 3;
        }

        // Pattern 2b: Dup + ArrayGet + Peek + Pop → ArrayGet + Peek
        if (i + 3 < instructions.len and
            std.meta.activeTag(instructions[i]) == .Dup and
            std.meta.activeTag(instructions[i + 1]) == .ArrayGet and
            std.meta.activeTag(instructions[i + 2]) == .Peek and
            std.meta.activeTag(instructions[i + 3]) == .Pop)
        {
            try optimized.append(instructions[i + 1]); // ArrayGet
            try optimized.append(instructions[i + 2]); // Peek
            self.redundant_eliminations += 1;
            return 4;
        }

        // Pattern 3: LoadVar + Dup + Peek + Pop → LoadVar + Peek
        if (i + 3 < instructions.len and
            std.meta.activeTag(instructions[i]) == .LoadVar and
            std.meta.activeTag(instructions[i + 1]) == .Dup and
            std.meta.activeTag(instructions[i + 2]) == .Peek and
            std.meta.activeTag(instructions[i + 3]) == .Pop)
        {
            try optimized.append(instructions[i]); // LoadVar
            try optimized.append(instructions[i + 2]); // Peek
            self.redundant_eliminations += 1;
            return 4;
        }

        // Pattern 4: Const + Pop → eliminate entirely
        if (i + 1 < instructions.len and
            std.meta.activeTag(instructions[i]) == .Const and
            std.meta.activeTag(instructions[i + 1]) == .Pop)
        {
            self.redundant_eliminations += 1;
            return 2; // Skip both instructions
        }

        // Pattern 5: Dup + Pop → eliminate entirely (when not part of peek pattern)
        if (i + 1 < instructions.len and
            std.meta.activeTag(instructions[i]) == .Dup and
            std.meta.activeTag(instructions[i + 1]) == .Pop)
        {
            self.redundant_eliminations += 1;
            return 2; // Skip both instructions
        }

        // Pattern 6: Multiple consecutive Pops → single Pop
        if (std.meta.activeTag(instructions[i]) == .Pop) {
            var pop_count: usize = 1;
            var j = i + 1;
            while (j < instructions.len and std.meta.activeTag(instructions[j]) == .Pop) {
                pop_count += 1;
                j += 1;
            }
            if (pop_count > 1) {
                try optimized.append(instructions[i]); // Single Pop
                self.redundant_eliminations += @intCast(pop_count - 1);
                return pop_count;
            }
        }

        return null;
    }

    //==================================================================
    // CATEGORY 2: ARITHMETIC OPTIMIZATIONS
    //==================================================================

    fn tryArithmeticOptimization(self: *PeepholeOptimizer, instructions: []HIRInstruction, i: usize, optimized: *std.ArrayList(HIRInstruction)) !?usize {

        // Pattern 1: Arithmetic identity operations (x + 0, x * 1, etc.)
        if (i + 2 < instructions.len and
            std.meta.activeTag(instructions[i + 1]) == .Const and
            std.meta.activeTag(instructions[i + 2]) == .Arith)
        {
            const const_instr = instructions[i + 1].Const;
            const arith_instr = instructions[i + 2].Arith;

            // Check for arithmetic identity operations
            switch (const_instr.value) {
                .int => |val| {
                    if (val == 0 and arith_instr.op == .Add) {
                        // x + 0 = x, eliminate Const 0 and Add
                        try optimized.append(instructions[i]); // Keep the other operand
                        self.arithmetic_optimizations += 1;
                        return 3;
                    }
                    if (val == 0 and arith_instr.op == .Sub) {
                        // x - 0 = x, eliminate Const 0 and Sub
                        try optimized.append(instructions[i]); // Keep the other operand
                        self.arithmetic_optimizations += 1;
                        return 3;
                    }
                    if (val == 1 and arith_instr.op == .Mul) {
                        // x * 1 = x, eliminate Const 1 and Mul
                        try optimized.append(instructions[i]); // Keep the other operand
                        self.arithmetic_optimizations += 1;
                        return 3;
                    }
                    if (val == 1 and arith_instr.op == .Div) {
                        // x / 1 = x, eliminate Const 1 and Div
                        try optimized.append(instructions[i]); // Keep the other operand
                        self.arithmetic_optimizations += 1;
                        return 3;
                    }
                    if (val == 0 and arith_instr.op == .Mul) {
                        // x * 0 = 0, replace with Const 0 (pop x, push 0)
                        try optimized.append(.Pop); // Remove x
                        try optimized.append(instructions[i + 1]); // Push 0
                        self.arithmetic_optimizations += 1;
                        return 3;
                    }
                },
                else => {},
            }
        }

        // Pattern 2: Strength reduction for multiplication by 2
        if (i + 2 < instructions.len and
            std.meta.activeTag(instructions[i + 1]) == .Const and
            std.meta.activeTag(instructions[i + 2]) == .Arith)
        {
            const const_instr = instructions[i + 1].Const;
            const arith_instr = instructions[i + 2].Arith;

            switch (const_instr.value) {
                .int => |val| {
                    if (val == 2 and arith_instr.op == .Mul) {
                        // x * 2 → x + x (addition faster than multiplication)
                        try optimized.append(instructions[i]); // First operand
                        try optimized.append(.Dup); // Duplicate it
                        try optimized.append(.{ .Arith = .{ .op = .Add, .operand_type = arith_instr.operand_type } });
                        self.arithmetic_optimizations += 1;
                        return 3;
                    }
                },
                else => {},
            }
        }

        return null;
    }

    //==================================================================
    // CATEGORY 3: VARIABLE ACCESS OPTIMIZATIONS
    //==================================================================

    fn tryVariableOptimization(self: *PeepholeOptimizer, instructions: []HIRInstruction, i: usize, optimized: *std.ArrayList(HIRInstruction)) !?usize {

        // Pattern 1: StoreVar + LoadVar (same variable) → StoreVar + Dup
        if (i + 1 < instructions.len and
            std.meta.activeTag(instructions[i]) == .StoreVar and
            std.meta.activeTag(instructions[i + 1]) == .LoadVar)
        {
            const store_instr = instructions[i].StoreVar;
            const load_instr = instructions[i + 1].LoadVar;

            // NEW: Do not apply across recent array-mutation sequences
            if (i > 0) {
                const prev_tag = std.meta.activeTag(instructions[i - 1]);
                if (prev_tag == .Swap or prev_tag == .ArrayPop or prev_tag == .ArrayPush or prev_tag == .ArraySet) {
                    // Skip optimization to preserve ordering semantics
                    return null;
                }
            }

            // Check if same variable
            if (store_instr.var_index == load_instr.var_index and
                std.mem.eql(u8, store_instr.var_name, load_instr.var_name))
            {
                // Replace LoadVar with Dup (value is already on stack from StoreVar)
                try optimized.append(.Dup); // Duplicate for StoreVar
                try optimized.append(instructions[i]); // StoreVar (consumes one copy)
                try optimized.append(.Dup); // Duplicate result for LoadVar
                self.variable_optimizations += 1;
                return 2;
            }
        }

        // Pattern 2: LoadVar same variable consecutively → LoadVar + Dup
        if (i + 1 < instructions.len and
            std.meta.activeTag(instructions[i]) == .LoadVar and
            std.meta.activeTag(instructions[i + 1]) == .LoadVar)
        {
            const load1 = instructions[i].LoadVar;
            const load2 = instructions[i + 1].LoadVar;

            if (load1.var_index == load2.var_index and
                std.mem.eql(u8, load1.var_name, load2.var_name))
            {
                // Replace second LoadVar with Dup
                try optimized.append(instructions[i]); // First LoadVar
                try optimized.append(.Dup); // Duplicate instead of second LoadVar
                self.variable_optimizations += 1;
                return 2;
            }
        }

        return null;
    }

    /// Reset all optimization counters
    pub fn resetCounters(self: *PeepholeOptimizer) void {
        self.redundant_eliminations = 0;
        self.arithmetic_optimizations = 0;
        self.stack_optimizations = 0;
        self.variable_optimizations = 0;
        self.control_flow_optimizations = 0;
    }

    /// Get total number of optimizations applied
    pub fn getTotalOptimizations(self: *PeepholeOptimizer) u32 {
        return self.redundant_eliminations +
            self.arithmetic_optimizations +
            self.stack_optimizations +
            self.variable_optimizations +
            self.control_flow_optimizations;
    }
};

/// Statistics about applied optimizations
pub const OptimizationStats = struct {
    redundant_eliminations: u32,
    arithmetic_optimizations: u32,
    stack_optimizations: u32,
    variable_optimizations: u32,
    control_flow_optimizations: u32,

    pub fn total(self: OptimizationStats) u32 {
        return self.redundant_eliminations +
            self.arithmetic_optimizations +
            self.stack_optimizations +
            self.variable_optimizations +
            self.control_flow_optimizations;
    }
};
