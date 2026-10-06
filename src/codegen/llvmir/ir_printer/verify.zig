const std = @import("std");

/// HIR verification at the emitter boundary (plan/type-authority.md, Phase A).
///
/// The emitter is the only stage that knows a value's representation: it
/// simulates the HIR stack machine and records, per slot, whether the value is
/// an unboxed scalar, a pointer, a string pair or a boxed `%DoxaValue`. An HIR
/// instruction that asks for more operands than the stack holds, or whose
/// annotation names a representation the operand does not have, is a compiler
/// bug in the stage that produced it. These checks turn such an instruction
/// into a failed compile that names the instruction, instead of a handler that
/// returns early or formats the operand into an instruction of another type.
///
/// A program that trips a check has found a bug in the compiler, never in
/// itself; the message says so.
pub fn Methods(comptime Ctx: type) type {
    const IRPrinter = Ctx.IRPrinter;
    const HIRInstruction = Ctx.HIRInstruction;
    const StackType = Ctx.StackType;
    const StackVal = Ctx.StackVal;

    return struct {
        pub const Error = error{MalformedHIR};

        /// Record the instruction about to be emitted so a failed check can
        /// name it. Called once per instruction by both dispatch loops.
        pub fn verifyEnter(self: *IRPrinter, function_name: []const u8, index: usize, inst: HIRInstruction) void {
            self.verify_function = function_name;
            self.verify_index = index;
            self.verify_tag = @tagName(std.meta.activeTag(inst));
        }

        /// Fail the compile over the current instruction.
        pub fn hirFault(self: *IRPrinter, comptime fmt: []const u8, args: anytype) Error {
            const detail = std.fmt.allocPrint(self.allocator, fmt, args) catch "(out of memory formatting the detail)";
            if (self.reporter) |reporter| {
                reporter.reportInternal(
                    "malformed HIR in '{s}', instruction #{d} ({s}): {s}. This is a compiler bug, not an error in the program",
                    .{ self.verify_function, self.verify_index, self.verify_tag, detail },
                    @src(),
                );
            } else {
                std.debug.print(
                    "Doxa: internal error: malformed HIR in '{s}', instruction #{d} ({s}): {s}. This is a compiler bug, not an error in the program.\n",
                    .{ self.verify_function, self.verify_index, self.verify_tag, detail },
                );
            }
            return error.MalformedHIR;
        }

        /// The labels control can actually arrive at through a jump, found by
        /// walking `instructions` from the top and following only jumps that
        /// are themselves reachable, to a fixed point (a loop's back edge is
        /// discovered on a later pass). A label outside this set that is not
        /// fallen into begins unreachable code: no predecessor defines its
        /// stack, so it is neither emitted nor verified.
        ///
        /// Only `Jump`, `JumpCond`, `Return`, `Halt` and `Unreachable` end a
        /// run of live code here. Treating fewer instructions as terminators
        /// than the emitter does can only make the set larger, never drop a
        /// label that is live.
        pub fn collectLiveJumpTargets(self: *IRPrinter, instructions: []const HIRInstruction) !std.StringHashMap(void) {
            var live = std.StringHashMap(void).init(self.allocator);
            errdefer live.deinit();
            var changed = true;
            while (changed) {
                changed = false;
                var alive = true;
                for (instructions) |inst| {
                    switch (inst) {
                        .Label => |lbl| {
                            if (!alive and live.contains(lbl.name)) alive = true;
                        },
                        .Jump => |j| {
                            if (alive and !live.contains(j.label)) {
                                try live.put(j.label, {});
                                changed = true;
                            }
                            alive = false;
                        },
                        .JumpCond => |jc| {
                            if (alive) {
                                if (!live.contains(jc.label_true)) {
                                    try live.put(jc.label_true, {});
                                    changed = true;
                                }
                                if (!live.contains(jc.label_false)) {
                                    try live.put(jc.label_false, {});
                                    changed = true;
                                }
                            }
                            alive = false;
                        },
                        .Return, .Halt, .Unreachable => alive = false,
                        else => {},
                    }
                }
            }
            return live;
        }

        /// The current instruction consumes `needed` operands.
        pub fn requireStack(self: *IRPrinter, stack: *const std.array_list.Managed(StackVal), needed: usize) Error!void {
            if (stack.items.len >= needed) return;
            return self.hirFault("needs {d} operand(s), the stack holds {d}", .{ needed, stack.items.len });
        }

        /// `val` is about to be formatted into an instruction typed `want`
        /// with no conversion in between, so it has to already have that
        /// representation.
        pub fn requireRepr(self: *IRPrinter, role: []const u8, val: StackVal, want: StackType) Error!void {
            if (val.ty == want) return;
            return self.hirFault("{s} operand is {s}, the instruction is lowered as {s}", .{ role, @tagName(val.ty), @tagName(want) });
        }

        /// `val` is one of the representations in `allowed`.
        pub fn requireReprIn(self: *IRPrinter, role: []const u8, val: StackVal, comptime allowed: []const StackType) Error!void {
            inline for (allowed) |candidate| {
                if (val.ty == candidate) return;
            }
            return self.hirFault("{s} operand is {s}, which this instruction has no lowering for", .{ role, @tagName(val.ty) });
        }
    };
}
