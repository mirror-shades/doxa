const std = @import("std");
const llvm = @import("llvm");
const LLVMCore = llvm.core;
const LLVMTypes = llvm.types;

const HIR = @import("../hir/soxa_types.zig");
const HIRValue = @import("../hir/soxa_values.zig").HIRValue;
const HIRInstruction = @import("../hir/soxa_instructions.zig").HIRInstruction;
const LLVMGenerator = @import("llvm.zig").LLVMGenerator;
const cast = @import("common/cast.zig");
const externs = @import("common/externs.zig");
const strings = @import("common/strings.zig");
const type_map = @import("common/types.zig");
const blocks = @import("common/blocks.zig");
const vars = @import("common/vars.zig");
const fn_common = @import("common/functions.zig");

var next_string_id: usize = 0; // unique IDs for string globals within this module
var next_block_id: usize = 0; // unique IDs for synthetic continuation blocks

fn byteSaturatingAdd(gen: *LLVMGenerator, a: LLVMTypes.LLVMValueRef, b: LLVMTypes.LLVMValueRef) LLVMTypes.LLVMValueRef {
    const i8_ty = LLVMCore.LLVMInt8TypeInContext(gen.context);
    const i16_ty = LLVMCore.LLVMInt16TypeInContext(gen.context);
    const a16 = LLVMCore.LLVMBuildZExt(gen.builder, cast.ensureI8(gen, a), i16_ty, "a.z16");
    const b16 = LLVMCore.LLVMBuildZExt(gen.builder, cast.ensureI8(gen, b), i16_ty, "b.z16");
    const sum16 = LLVMCore.LLVMBuildAdd(gen.builder, a16, b16, "add16");
    const max255 = LLVMCore.LLVMConstInt(i16_ty, 255, @intFromBool(false));
    const gt = LLVMCore.LLVMBuildICmp(gen.builder, LLVMTypes.LLVMIntPredicate.LLVMIntUGT, sum16, max255, "gt255");
    const clamped = LLVMCore.LLVMBuildSelect(gen.builder, gt, max255, sum16, "sel");
    return LLVMCore.LLVMBuildTrunc(gen.builder, clamped, i8_ty, "to.i8");
}

fn byteSaturatingSub(gen: *LLVMGenerator, a: LLVMTypes.LLVMValueRef, b: LLVMTypes.LLVMValueRef) LLVMTypes.LLVMValueRef {
    const i8_ty = LLVMCore.LLVMInt8TypeInContext(gen.context);
    const a8 = cast.ensureI8(gen, a);
    const b8 = cast.ensureI8(gen, b);
    const ge = LLVMCore.LLVMBuildICmp(gen.builder, LLVMTypes.LLVMIntPredicate.LLVMIntUGE, a8, b8, "ge");
    const diff = LLVMCore.LLVMBuildSub(gen.builder, a8, b8, "sub8");
    const zero = LLVMCore.LLVMConstInt(i8_ty, 0, @intFromBool(false));
    return LLVMCore.LLVMBuildSelect(gen.builder, ge, diff, zero, "sel");
}

fn byteSaturatingMul(gen: *LLVMGenerator, a: LLVMTypes.LLVMValueRef, b: LLVMTypes.LLVMValueRef) LLVMTypes.LLVMValueRef {
    const i8_ty = LLVMCore.LLVMInt8TypeInContext(gen.context);
    const i32_ty = LLVMCore.LLVMInt32TypeInContext(gen.context);
    const a32 = LLVMCore.LLVMBuildZExt(gen.builder, cast.ensureI8(gen, a), i32_ty, "a.z32");
    const b32 = LLVMCore.LLVMBuildZExt(gen.builder, cast.ensureI8(gen, b), i32_ty, "b.z32");
    const prod32 = LLVMCore.LLVMBuildMul(gen.builder, a32, b32, "mul32");
    const max255 = LLVMCore.LLVMConstInt(i32_ty, 255, @intFromBool(false));
    const gt = LLVMCore.LLVMBuildICmp(gen.builder, LLVMTypes.LLVMIntPredicate.LLVMIntUGT, prod32, max255, "gt255");
    const sel = LLVMCore.LLVMBuildSelect(gen.builder, gt, max255, prod32, "sel");
    const trunc = LLVMCore.LLVMBuildTrunc(gen.builder, sel, i8_ty, "to.i8");
    return trunc;
}

fn findLabelIndex(hir: *const HIR.HIRProgram, label: []const u8) ?usize {
    for (hir.instructions, 0..) |inst, idx| {
        if (inst == .Label and std.mem.eql(u8, inst.Label.name, label)) return idx;
    }
    return null;
}

fn coerceToType(gen: *LLVMGenerator, v: LLVMTypes.LLVMValueRef, target_ty: LLVMTypes.LLVMTypeRef) LLVMTypes.LLVMValueRef {
    const kind = LLVMCore.LLVMGetTypeKind(target_ty);
    switch (kind) {
        LLVMTypes.LLVMTypeKind.LLVMIntegerTypeKind => {
            const w = LLVMCore.LLVMGetIntTypeWidth(target_ty);
            if (w == 1) return cast.ensureI1(gen, v);
            if (w == 8) return cast.ensureI8(gen, v);
            return cast.ensureI64(gen, v);
        },
        LLVMTypes.LLVMTypeKind.LLVMDoubleTypeKind => return cast.ensureF64(gen, v),
        else => return v,
    }
}

// Function body emission moved to common/functions.zig

pub fn translateToLLVM(hir: *const HIR.HIRProgram, generator: *LLVMGenerator) !void {
    const i32_ty = LLVMCore.LLVMInt32TypeInContext(generator.context);
    const fn_ty = LLVMCore.LLVMFunctionType(i32_ty, null, 0, @intFromBool(false));
    const main_fn = LLVMCore.LLVMAddFunction(generator.module, "main", fn_ty);
    const entry = LLVMCore.LLVMAppendBasicBlockInContext(generator.context, main_fn, "entry");
    LLVMCore.LLVMPositionBuilderAtEnd(generator.builder, entry);

    var stack = std.array_list.Managed(LLVMTypes.LLVMValueRef).init(generator.allocator);
    defer stack.deinit();

    var bm = blocks.BlockManager.init(generator.allocator, generator, main_fn, &next_block_id);
    defer bm.deinit();

    var var_store = vars.VarStore.init(generator.allocator, generator, entry);
    defer var_store.deinit();

    // Predeclare all functions with correct param/return types (alias => pointer)
    var function_map = std.StringHashMap(LLVMTypes.LLVMValueRef).init(generator.allocator);
    defer function_map.deinit();

    var func_start_labels = std.StringHashMap(bool).init(generator.allocator);
    defer func_start_labels.deinit();

    try fn_common.predeclareFunctions(hir, generator, &function_map, &func_start_labels);

    // Determine where functions begin; only emit top-level before that
    const func_section_start: usize = fn_common.findFunctionsSectionStart(hir, &func_start_labels);

    for (hir.instructions[0..func_section_start]) |inst| {
        switch (inst) {
            .Const => |c| {
                const idx = c.constant_id;
                if (idx >= hir.constant_pool.len) continue;
                const hv = hir.constant_pool[idx];
                switch (hv) {
                    .string => |s| {
                        const ptr = try strings.createStringPtr(generator, s, &next_string_id);
                        try stack.append(ptr);
                    },
                    .int => |i| {
                        const v = LLVMCore.LLVMConstInt(LLVMCore.LLVMInt64TypeInContext(generator.context), @intCast(i), @intFromBool(true));
                        try stack.append(v);
                    },
                    .byte => |b| {
                        const v = LLVMCore.LLVMConstInt(LLVMCore.LLVMInt8TypeInContext(generator.context), b, @intFromBool(false));
                        try stack.append(v);
                    },
                    .float => |f| {
                        const v = LLVMCore.LLVMConstReal(LLVMCore.LLVMDoubleTypeInContext(generator.context), f);
                        try stack.append(v);
                    },
                    .tetra => |t| {
                        const v = LLVMCore.LLVMConstInt(LLVMCore.LLVMIntTypeInContext(generator.context, 2), t, @intFromBool(false));
                        try stack.append(v);
                    },
                    else => {},
                }
            },
            .Arith => |a| {
                if (stack.items.len < 2) continue;
                const right = stack.items[stack.items.len - 1];
                const left = stack.items[stack.items.len - 2];
                stack.items.len -= 2;

                var result: LLVMTypes.LLVMValueRef = null;
                switch (a.operand_type) {
                    .Float => {
                        const lf = cast.ensureF64(generator, left);
                        const rf = cast.ensureF64(generator, right);
                        result = switch (a.op) {
                            .Add => LLVMCore.LLVMBuildFAdd(generator.builder, lf, rf, "fadd"),
                            .Sub => LLVMCore.LLVMBuildFSub(generator.builder, lf, rf, "fsub"),
                            .Mul => LLVMCore.LLVMBuildFMul(generator.builder, lf, rf, "fmul"),
                            .Div => LLVMCore.LLVMBuildFDiv(generator.builder, lf, rf, "fdiv"),
                            .Mod => LLVMCore.LLVMBuildFRem(generator.builder, lf, rf, "frem"),
                            .Pow => blk: {
                                const pow_fn = externs.getOrCreatePow(generator);
                                var args = [_]LLVMTypes.LLVMValueRef{ lf, rf };
                                const f64_ty = LLVMCore.LLVMDoubleTypeInContext(generator.context);
                                var ptys = [_]LLVMTypes.LLVMTypeRef{ f64_ty, f64_ty };
                                const pty = LLVMCore.LLVMFunctionType(f64_ty, &ptys, 2, @intFromBool(false));
                                break :blk LLVMCore.LLVMBuildCall2(generator.builder, pty, pow_fn, &args, 2, "pow");
                            },
                        };
                    },
                    .Int => {
                        const li64 = cast.ensureI64(generator, left);
                        const ri64 = cast.ensureI64(generator, right);
                        result = switch (a.op) {
                            .Add => LLVMCore.LLVMBuildAdd(generator.builder, li64, ri64, "add"),
                            .Sub => LLVMCore.LLVMBuildSub(generator.builder, li64, ri64, "sub"),
                            .Mul => LLVMCore.LLVMBuildMul(generator.builder, li64, ri64, "mul"),
                            .Div => blk_div: {
                                const lf = cast.ensureF64(generator, li64);
                                const rf = cast.ensureF64(generator, ri64);
                                break :blk_div LLVMCore.LLVMBuildFDiv(generator.builder, lf, rf, "idiv.f");
                            },
                            .Mod => LLVMCore.LLVMBuildSRem(generator.builder, li64, ri64, "srem"),
                            .Pow => blk_pow: {
                                const lf = cast.ensureF64(generator, li64);
                                const rf = cast.ensureF64(generator, ri64);
                                const pow_fn = externs.getOrCreatePow(generator);
                                var args = [_]LLVMTypes.LLVMValueRef{ lf, rf };
                                const f64_ty = LLVMCore.LLVMDoubleTypeInContext(generator.context);
                                var ptys = [_]LLVMTypes.LLVMTypeRef{ f64_ty, f64_ty };
                                const pty = LLVMCore.LLVMFunctionType(f64_ty, &ptys, 2, @intFromBool(false));
                                const dres = LLVMCore.LLVMBuildCall2(generator.builder, pty, pow_fn, &args, 2, "pow");
                                break :blk_pow LLVMCore.LLVMBuildFPToSI(generator.builder, dres, LLVMCore.LLVMInt64TypeInContext(generator.context), "pow.to.i64");
                            },
                        };
                    },
                    .Byte => {
                        // Byte semantics are saturating for + and *; clamp for pow; 0 on div/mod by zero
                        result = switch (a.op) {
                            .Add => byteSaturatingAdd(generator, left, right),
                            .Sub => byteSaturatingSub(generator, left, right),
                            .Mul => byteSaturatingMul(generator, left, right),
                            .Div => blk_bdiv: {
                                const a8 = cast.ensureI8(generator, left);
                                const b8 = cast.ensureI8(generator, right);
                                const zero8 = LLVMCore.LLVMConstInt(LLVMCore.LLVMInt8TypeInContext(generator.context), 0, @intFromBool(false));
                                const is_zero = LLVMCore.LLVMBuildICmp(generator.builder, LLVMTypes.LLVMIntPredicate.LLVMIntEQ, b8, zero8, "is0");
                                const quot = LLVMCore.LLVMBuildUDiv(generator.builder, a8, b8, "udiv8");
                                break :blk_bdiv LLVMCore.LLVMBuildSelect(generator.builder, is_zero, zero8, quot, "sel");
                            },
                            .Mod => blk_bmod: {
                                const a8 = cast.ensureI8(generator, left);
                                const b8 = cast.ensureI8(generator, right);
                                const zero8 = LLVMCore.LLVMConstInt(LLVMCore.LLVMInt8TypeInContext(generator.context), 0, @intFromBool(false));
                                const is_zero = LLVMCore.LLVMBuildICmp(generator.builder, LLVMTypes.LLVMIntPredicate.LLVMIntEQ, b8, zero8, "is0");
                                const rem = LLVMCore.LLVMBuildURem(generator.builder, a8, b8, "urem8");
                                break :blk_bmod LLVMCore.LLVMBuildSelect(generator.builder, is_zero, zero8, rem, "sel");
                            },
                            .Pow => blk_bpow: {
                                const f64_ty = LLVMCore.LLVMDoubleTypeInContext(generator.context);
                                const a_f = cast.ensureF64(generator, cast.ensureI8(generator, left));
                                const b_f = cast.ensureF64(generator, cast.ensureI8(generator, right));
                                const pow_fn = externs.getOrCreatePow(generator);
                                var args = [_]LLVMTypes.LLVMValueRef{ a_f, b_f };
                                var ptys = [_]LLVMTypes.LLVMTypeRef{ f64_ty, f64_ty };
                                const pty = LLVMCore.LLVMFunctionType(f64_ty, &ptys, 2, @intFromBool(false));
                                const dres = LLVMCore.LLVMBuildCall2(generator.builder, pty, pow_fn, &args, 2, "pow");
                                const as_u32 = LLVMCore.LLVMBuildFPToUI(generator.builder, dres, i32_ty, "fp2u32");
                                const max255 = LLVMCore.LLVMConstInt(i32_ty, 255, @intFromBool(false));
                                const gt = LLVMCore.LLVMBuildICmp(generator.builder, LLVMTypes.LLVMIntPredicate.LLVMIntUGT, as_u32, max255, "gt255");
                                const sel = LLVMCore.LLVMBuildSelect(generator.builder, gt, max255, as_u32, "sel");
                                const i8_ty = LLVMCore.LLVMInt8TypeInContext(generator.context);
                                break :blk_bpow LLVMCore.LLVMBuildTrunc(generator.builder, sel, i8_ty, "to.i8");
                            },
                        };
                    },
                    else => {
                        // Unsupported operand type for this simple translator; push back left to avoid stack underflow in later ops
                        result = left;
                    },
                }

                try stack.append(result);
            },
            .Convert => |conv| {
                if (stack.items.len < 1) continue;
                const v = stack.items[stack.items.len - 1];
                stack.items.len -= 1;

                var out: LLVMTypes.LLVMValueRef = v;
                switch (conv.to_type) {
                    .Int => {
                        switch (conv.from_type) {
                            .Int => {
                                out = cast.ensureI64(generator, v);
                            },
                            .Byte => {
                                const i64_ty = LLVMCore.LLVMInt64TypeInContext(generator.context);
                                out = LLVMCore.LLVMBuildZExt(generator.builder, cast.ensureI8(generator, v), i64_ty, "zext.i64");
                            },
                            .Float => {
                                const i64_ty = LLVMCore.LLVMInt64TypeInContext(generator.context);
                                out = LLVMCore.LLVMBuildFPToSI(generator.builder, cast.ensureF64(generator, v), i64_ty, "fp2si.i64");
                            },
                            else => {},
                        }
                    },
                    .Byte => {
                        const i8_ty = LLVMCore.LLVMInt8TypeInContext(generator.context);
                        switch (conv.from_type) {
                            .Int => {
                                out = LLVMCore.LLVMBuildTrunc(generator.builder, cast.ensureI64(generator, v), i8_ty, "trunc.i8");
                            },
                            .Byte => {
                                out = cast.ensureI8(generator, v);
                            },
                            .Float => {
                                out = LLVMCore.LLVMBuildFPToUI(generator.builder, cast.ensureF64(generator, v), i8_ty, "fp2ui.i8");
                            },
                            else => {},
                        }
                    },
                    .Float => {
                        const f64_ty = LLVMCore.LLVMDoubleTypeInContext(generator.context);
                        switch (conv.from_type) {
                            .Int => {
                                out = LLVMCore.LLVMBuildSIToFP(generator.builder, cast.ensureI64(generator, v), f64_ty, "si2fp.f64");
                            },
                            .Byte => {
                                const i8v = cast.ensureI8(generator, v);
                                out = LLVMCore.LLVMBuildUIToFP(generator.builder, i8v, f64_ty, "ui2fp.f64");
                            },
                            .Float => {
                                out = cast.ensureF64(generator, v);
                            },
                            else => {},
                        }
                    },
                    else => {},
                }

                try stack.append(out);
            },
            .Compare => |cmp| {
                if (stack.items.len < 2) continue;
                const right = stack.items[stack.items.len - 1];
                const left = stack.items[stack.items.len - 2];
                stack.items.len -= 2;

                var result: LLVMTypes.LLVMValueRef = null;
                switch (cmp.operand_type) {
                    .Int => {
                        const li = cast.ensureI64(generator, left);
                        const ri = cast.ensureI64(generator, right);
                        const pred = switch (cmp.op) {
                            .Eq => LLVMTypes.LLVMIntPredicate.LLVMIntEQ,
                            .Ne => LLVMTypes.LLVMIntPredicate.LLVMIntNE,
                            .Lt => LLVMTypes.LLVMIntPredicate.LLVMIntSLT,
                            .Le => LLVMTypes.LLVMIntPredicate.LLVMIntSLE,
                            .Gt => LLVMTypes.LLVMIntPredicate.LLVMIntSGT,
                            .Ge => LLVMTypes.LLVMIntPredicate.LLVMIntSGE,
                        };
                        result = LLVMCore.LLVMBuildICmp(generator.builder, pred, li, ri, "icmp");
                    },
                    .Byte => {
                        const li8 = cast.ensureI8(generator, left);
                        const ri8 = cast.ensureI8(generator, right);
                        const pred = switch (cmp.op) {
                            .Eq => LLVMTypes.LLVMIntPredicate.LLVMIntEQ,
                            .Ne => LLVMTypes.LLVMIntPredicate.LLVMIntNE,
                            .Lt => LLVMTypes.LLVMIntPredicate.LLVMIntULT,
                            .Le => LLVMTypes.LLVMIntPredicate.LLVMIntULE,
                            .Gt => LLVMTypes.LLVMIntPredicate.LLVMIntUGT,
                            .Ge => LLVMTypes.LLVMIntPredicate.LLVMIntUGE,
                        };
                        result = LLVMCore.LLVMBuildICmp(generator.builder, pred, li8, ri8, "icmp");
                    },
                    .Float => {
                        const lf = cast.ensureF64(generator, left);
                        const rf = cast.ensureF64(generator, right);
                        const pred = switch (cmp.op) {
                            .Eq => LLVMTypes.LLVMRealPredicate.LLVMRealOEQ,
                            .Ne => LLVMTypes.LLVMRealPredicate.LLVMRealONE,
                            .Lt => LLVMTypes.LLVMRealPredicate.LLVMRealOLT,
                            .Le => LLVMTypes.LLVMRealPredicate.LLVMRealOLE,
                            .Gt => LLVMTypes.LLVMRealPredicate.LLVMRealOGT,
                            .Ge => LLVMTypes.LLVMRealPredicate.LLVMRealOGE,
                        };
                        result = LLVMCore.LLVMBuildFCmp(generator.builder, pred, lf, rf, "fcmp");
                    },
                    else => {
                        // Fallback: equality for pointers (e.g., strings) if needed in future
                        result = LLVMCore.LLVMConstInt(LLVMCore.LLVMInt1TypeInContext(generator.context), 0, @intFromBool(false));
                    },
                }

                try stack.append(result);
            },
            .LogicalOp => |lop| {
                const i2_ty = LLVMCore.LLVMIntTypeInContext(generator.context, 2);
                switch (lop.op) {
                    .Not => {
                        if (stack.items.len < 1) continue;
                        const a = stack.items[stack.items.len - 1];
                        stack.items.len -= 1;
                        const a2 = LLVMCore.LLVMBuildTrunc(generator.builder, cast.ensureI64(generator, a), i2_ty, "to.i2");
                        const one = LLVMCore.LLVMConstInt(i2_ty, 1, @intFromBool(false));
                        const two = LLVMCore.LLVMConstInt(i2_ty, 2, @intFromBool(false));
                        const low = LLVMCore.LLVMBuildAnd(generator.builder, a2, one, "tetra.low");
                        const high = LLVMCore.LLVMBuildAnd(generator.builder, a2, two, "tetra.high");
                        const low_to_high = LLVMCore.LLVMBuildShl(generator.builder, low, one, "tetra.l2h");
                        const high_to_low = LLVMCore.LLVMBuildLShr(generator.builder, high, one, "tetra.h2l");
                        const res = LLVMCore.LLVMBuildOr(generator.builder, low_to_high, high_to_low, "tetra.not");
                        try stack.append(res);
                    },
                    else => {
                        if (stack.items.len < 2) continue;
                        const b = stack.items[stack.items.len - 1];
                        const a = stack.items[stack.items.len - 2];
                        stack.items.len -= 2;
                        const a2 = LLVMCore.LLVMBuildTrunc(generator.builder, cast.ensureI64(generator, a), i2_ty, "a.i2");
                        const b2 = LLVMCore.LLVMBuildTrunc(generator.builder, cast.ensureI64(generator, b), i2_ty, "b.i2");
                        const one = LLVMCore.LLVMConstInt(i2_ty, 1, @intFromBool(false));
                        const two = LLVMCore.LLVMConstInt(i2_ty, 2, @intFromBool(false));
                        const i1_ty = LLVMCore.LLVMInt1TypeInContext(generator.context);
                        const a_is_true = blk_a: {
                            const is_one = LLVMCore.LLVMBuildICmp(generator.builder, LLVMTypes.LLVMIntPredicate.LLVMIntEQ, a2, one, "a.eq1");
                            const is_two = LLVMCore.LLVMBuildICmp(generator.builder, LLVMTypes.LLVMIntPredicate.LLVMIntEQ, a2, two, "a.eq2");
                            break :blk_a LLVMCore.LLVMBuildOr(generator.builder, is_one, is_two, "a.true");
                        };
                        const b_is_true = blk_b: {
                            const is_one = LLVMCore.LLVMBuildICmp(generator.builder, LLVMTypes.LLVMIntPredicate.LLVMIntEQ, b2, one, "b.eq1");
                            const is_two = LLVMCore.LLVMBuildICmp(generator.builder, LLVMTypes.LLVMIntPredicate.LLVMIntEQ, b2, two, "b.eq2");
                            break :blk_b LLVMCore.LLVMBuildOr(generator.builder, is_one, is_two, "b.true");
                        };
                        const and_i1 = LLVMCore.LLVMBuildAnd(generator.builder, a_is_true, b_is_true, "and");
                        const or_i1 = LLVMCore.LLVMBuildOr(generator.builder, a_is_true, b_is_true, "or");
                        const xor_i1 = LLVMCore.LLVMBuildXor(generator.builder, a_is_true, b_is_true, "xor");
                        const not_b = LLVMCore.LLVMBuildXor(generator.builder, b_is_true, LLVMCore.LLVMConstInt(i1_ty, 1, @intFromBool(false)), "not.b");
                        const implies_i1 = LLVMCore.LLVMBuildOr(generator.builder, not_b, a_is_true, "impl");
                        const iff_i1 = LLVMCore.LLVMBuildXor(generator.builder, xor_i1, LLVMCore.LLVMConstInt(i1_ty, 1, @intFromBool(false)), "iff");
                        const nand_i1 = LLVMCore.LLVMBuildXor(generator.builder, and_i1, LLVMCore.LLVMConstInt(i1_ty, 1, @intFromBool(false)), "nand");
                        const nor_i1 = LLVMCore.LLVMBuildXor(generator.builder, or_i1, LLVMCore.LLVMConstInt(i1_ty, 1, @intFromBool(false)), "nor");
                        const sel_i1 = switch (lop.op) {
                            .And => and_i1,
                            .Or => or_i1,
                            .Xor => xor_i1,
                            .Iff => iff_i1,
                            .Nand => nand_i1,
                            .Nor => nor_i1,
                            .Implies => implies_i1,
                            else => LLVMCore.LLVMConstInt(i1_ty, 0, @intFromBool(false)),
                        };
                        const out_i2 = LLVMCore.LLVMBuildZExt(generator.builder, sel_i1, i2_ty, "i1.to.i2");
                        try stack.append(out_i2);
                    },
                }
            },
            .Dup => {
                if (stack.items.len < 1) continue;
                const v = stack.items[stack.items.len - 1];
                try stack.append(v);
            },
            .Pop => {
                if (stack.items.len > 0) {
                    stack.items.len -= 1;
                }
            },
            .Swap => {
                if (stack.items.len >= 2) {
                    const a = stack.items[stack.items.len - 1];
                    const b = stack.items[stack.items.len - 2];
                    stack.items[stack.items.len - 2] = a;
                    stack.items[stack.items.len - 1] = b;
                }
            },
            .Print => {
                if (stack.items.len > 0) {
                    const v = stack.items[stack.items.len - 1];
                    stack.items.len -= 1;
                    @import("common/print.zig").printValue(generator, v, &next_string_id);
                }
            },
            .PrintBegin => {},
            .PrintStr => |p| {
                if (p.const_id < hir.constant_pool.len) {
                    const hv = hir.constant_pool[p.const_id];
                    if (hv == .string) {
                        @import("common/print.zig").printLiteralString(generator, hv.string, &next_string_id);
                    }
                }
            },
            .PrintVal => {
                if (stack.items.len > 0) {
                    const v = stack.items[stack.items.len - 1];
                    stack.items.len -= 1;
                    @import("common/print.zig").printValue(generator, v, &next_string_id);
                }
            },
            .Peek => |pk| {
                // In release, drop peek entirely
                if (!generator.debug_peek) {
                    if (stack.items.len > 0) {
                        // Pop the peeked value and push it back to preserve semantics
                        const v = stack.items[stack.items.len - 1];
                        stack.items.len -= 1;
                        try stack.append(v);
                    }
                    continue; // Skip emitting peek but keep translating subsequent instructions
                }
                if (stack.items.len > 0) {
                    // Pop value to peek and print with prefix: [file:line:col] name :: type is value
                    const v = stack.items[stack.items.len - 1];
                    stack.items.len -= 1;

                    // Build prefix
                    var prefix_buf: [256]u8 = undefined;
                    var fbs = std.io.fixedBufferStream(&prefix_buf);
                    var w = fbs.writer();
                    if (pk.location) |loc| {
                        const file = loc.file;
                        const line = loc.range.start_line;
                        const col = loc.range.start_col;
                        try w.print("[{s}:{d}:{d}] ", .{ file, line, col });
                    }
                    if (pk.name) |nm| {
                        try w.print("{s} :: ", .{nm});
                    }

                    // Determine type tag for display
                    const kind = LLVMCore.LLVMGetTypeKind(LLVMCore.LLVMTypeOf(v));
                    const type_tag = switch (kind) {
                        LLVMTypes.LLVMTypeKind.LLVMIntegerTypeKind => "int",
                        LLVMTypes.LLVMTypeKind.LLVMDoubleTypeKind, LLVMTypes.LLVMTypeKind.LLVMFloatTypeKind => "float",
                        LLVMTypes.LLVMTypeKind.LLVMPointerTypeKind => "string",
                        else => "value",
                    };
                    try w.print("{s} is ", .{type_tag});
                    const prefix_len = fbs.pos;
                    const prefix_slice = prefix_buf[0..prefix_len];

                    @import("common/print.zig").printLiteralString(generator, prefix_slice, &next_string_id);
                    @import("common/print.zig").printValue(generator, v, &next_string_id);
                    @import("common/print.zig").printNewline(generator, &next_string_id);

                    // Push original value back on the stack
                    try stack.append(v);
                }
            },
            .PrintNewline => {
                @import("common/print.zig").printNewline(generator, &next_string_id);
            },
            .PrintEnd => {},
            .Halt => {
                const zero_ret = LLVMCore.LLVMConstInt(i32_ty, 0, @intFromBool(false));
                _ = LLVMCore.LLVMBuildRet(generator.builder, zero_ret);
                break;
            },
            .Label => |lbl| {
                try bm.enterLabel(lbl.name);
            },
            .Jump => |j| {
                try bm.branchTo(j.label);
            },
            .JumpCond => |jc| {
                if (stack.items.len < 1) continue;
                const cond_v = stack.items[stack.items.len - 1];
                stack.items.len -= 1;
                const cond_i1 = cast.ensureI1(generator, cond_v);
                try bm.branchCond(cond_i1, jc.label_true, jc.label_false);
            },
            .StoreVar => |sv| {
                if (stack.items.len < 1) continue;
                var v = stack.items[stack.items.len - 1];
                stack.items.len -= 1;

                const key = switch (sv.scope_kind) {
                    HIR.ScopeKind.Local => try std.fmt.allocPrint(generator.allocator, "l:{s}", .{sv.var_name}),
                    HIR.ScopeKind.GlobalLocal => try std.fmt.allocPrint(generator.allocator, "g:{s}", .{sv.var_name}),
                    HIR.ScopeKind.ModuleGlobal => blk: {
                        const mod = sv.module_context orelse "";
                        break :blk try std.fmt.allocPrint(generator.allocator, "m:{s}.{s}", .{ mod, sv.var_name });
                    },
                    else => try std.fmt.allocPrint(generator.allocator, "u:{s}", .{sv.var_name}),
                };

                const target_ty = type_map.mapHIRTypeToLLVM(generator, sv.expected_type);
                v = coerceToType(generator, v, target_ty);

                try var_store.store(key, v, target_ty, sv.scope_kind == HIR.ScopeKind.Local, sv.var_name);
            },
            .LoadVar => |lv| {
                const key = switch (lv.scope_kind) {
                    HIR.ScopeKind.Local => try std.fmt.allocPrint(generator.allocator, "l:{s}", .{lv.var_name}),
                    HIR.ScopeKind.GlobalLocal => try std.fmt.allocPrint(generator.allocator, "g:{s}", .{lv.var_name}),
                    HIR.ScopeKind.ModuleGlobal => blk: {
                        const mod = lv.module_context orelse "";
                        break :blk try std.fmt.allocPrint(generator.allocator, "m:{s}.{s}", .{ mod, lv.var_name });
                    },
                    else => try std.fmt.allocPrint(generator.allocator, "u:{s}", .{lv.var_name}),
                };

                const target_ty = LLVMCore.LLVMInt64TypeInContext(generator.context);
                const loaded = try var_store.load(key, target_ty, lv.var_name);
                try stack.append(loaded);
            },
            .StoreConst => |sc| {
                if (stack.items.len < 1) continue;
                const v = stack.items[stack.items.len - 1];
                stack.items.len -= 1;

                const key = switch (sc.scope_kind) {
                    HIR.ScopeKind.GlobalLocal => try std.fmt.allocPrint(generator.allocator, "g:{s}", .{sc.var_name}),
                    HIR.ScopeKind.ModuleGlobal => blk: {
                        const mod = sc.module_context orelse "";
                        break :blk try std.fmt.allocPrint(generator.allocator, "m:{s}.{s}", .{ mod, sc.var_name });
                    },
                    else => try std.fmt.allocPrint(generator.allocator, "g:{s}", .{sc.var_name}),
                };

                try var_store.defineConstGlobal(key, v);
            },
            .PushStorageId => |pstid| {
                // Top-level alias arguments should reference the actual variable storage.
                // Prefer GlobalLocal for script-level variables.
                const key = try std.fmt.allocPrint(generator.allocator, "g:{s}", .{pstid.var_name});
                const default_ty = LLVMCore.LLVMInt64TypeInContext(generator.context);
                const entry_ptr = try var_store.getOrCreatePtr(key, default_ty, false, pstid.var_name);
                try stack.append(entry_ptr.ptr);
            },
            .Call => |c| {
                const f_entry = function_map.get(c.qualified_name) orelse null;
                if (f_entry == null) continue;
                const callee = f_entry.?;
                const argc: usize = @intCast(c.arg_count);
                if (stack.items.len < argc) continue;
                var args = try generator.allocator.alloc(LLVMTypes.LLVMValueRef, argc);
                defer generator.allocator.free(args);
                var ai: usize = 0;
                while (ai < argc) : (ai += 1) {
                    const v2 = stack.items[stack.items.len - 1];
                    stack.items.len -= 1;
                    args[argc - 1 - ai] = v2;
                }
                // Coerce/cast arguments to match callee parameter types (including alias-as-pointer)
                var callee_info: ?HIR.HIRProgram.HIRFunction = null;
                for (hir.function_table) |finfo| {
                    if (std.mem.eql(u8, finfo.qualified_name, c.qualified_name)) {
                        callee_info = finfo;
                        break;
                    }
                }
                if (callee_info) |fi| {
                    var idx: usize = 0;
                    while (idx < args.len and idx < fi.param_types.len) : (idx += 1) {
                        const pt = fi.param_types[idx];
                        if (fi.param_is_alias[idx]) {
                            const elem = type_map.mapHIRTypeToLLVM(generator, pt);
                            const want_ptr_ty = LLVMCore.LLVMPointerType(elem, 0);
                            const arg_ty = LLVMCore.LLVMTypeOf(args[idx]);
                            if (LLVMCore.LLVMGetTypeKind(arg_ty) == LLVMTypes.LLVMTypeKind.LLVMPointerTypeKind) {
                                if (arg_ty != want_ptr_ty) {
                                    args[idx] = LLVMCore.LLVMBuildBitCast(generator.builder, args[idx], want_ptr_ty, "arg.ptr.cast");
                                }
                            } else {
                                // If value not a pointer, allocate a local and pass its pointer
                                const entry_block = LLVMCore.LLVMGetInsertBlock(generator.builder);
                                // Create an alloca in entry for stable address
                                LLVMCore.LLVMPositionBuilderAtEnd(generator.builder, entry);
                                const ptr = LLVMCore.LLVMBuildAlloca(generator.builder, elem, "alias.arg.alloca");
                                LLVMCore.LLVMPositionBuilderAtEnd(generator.builder, entry_block);
                                const coerced = coerceToType(generator, args[idx], elem);
                                _ = LLVMCore.LLVMBuildStore(generator.builder, coerced, ptr);
                                args[idx] = ptr;
                            }
                        } else {
                            const tgt = type_map.mapHIRTypeToLLVM(generator, pt);
                            args[idx] = coerceToType(generator, args[idx], tgt);
                        }
                    }
                }
                var fnty: LLVMTypes.LLVMTypeRef = undefined;
                if (callee_info) |fi2| {
                    const ret_ty = type_map.mapHIRTypeToLLVM(generator, fi2.return_type);
                    var pbuf = try generator.allocator.alloc(LLVMTypes.LLVMTypeRef, fi2.param_types.len);
                    defer generator.allocator.free(pbuf);
                    for (fi2.param_types, 0..) |pt, pi| {
                        const base = type_map.mapHIRTypeToLLVM(generator, pt);
                        pbuf[pi] = if (fi2.param_is_alias[pi]) LLVMCore.LLVMPointerType(base, 0) else base;
                    }
                    fnty = LLVMCore.LLVMFunctionType(ret_ty, if (pbuf.len == 0) null else pbuf.ptr, @intCast(pbuf.len), @intFromBool(false));
                } else {
                    // Best-effort fallback
                    fnty = LLVMCore.LLVMFunctionType(LLVMCore.LLVMVoidTypeInContext(generator.context), null, 0, @intFromBool(false));
                }
                const res = LLVMCore.LLVMBuildCall2(generator.builder, fnty, callee, if (argc == 0) null else args.ptr, @intCast(argc), "");
                if (c.return_type != .Nothing) {
                    try stack.append(res);
                }
            },
            else => {},
        }
    }

    // Emit function bodies
    for (hir.function_table) |f| {
        try fn_common.emitFunctionBody(hir, generator, &function_map, f, &func_start_labels, &next_block_id);
    }

    const insert_block = LLVMCore.LLVMGetInsertBlock(generator.builder);
    if (insert_block != null and LLVMCore.LLVMGetBasicBlockTerminator(insert_block) == null) {
        const zero_ret = LLVMCore.LLVMConstInt(i32_ty, 0, @intFromBool(false));
        _ = LLVMCore.LLVMBuildRet(generator.builder, zero_ret);
    }
}
