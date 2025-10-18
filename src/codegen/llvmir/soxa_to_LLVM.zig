const std = @import("std");
const llvm = @import("llvm");
const LLVMCore = llvm.core;
const LLVMTypes = llvm.types;

const HIR = @import("../hir/soxa_types.zig");
const HIRValue = @import("../hir/soxa_values.zig").HIRValue;
const HIRInstruction = @import("../hir/soxa_instructions.zig").HIRInstruction;
const LLVMGenerator = @import("llvm.zig").LLVMGenerator;

var next_string_id: usize = 0; // unique IDs for string globals within this module
var next_block_id: usize = 0; // unique IDs for synthetic continuation blocks

fn mapHIRTypeToLLVM(gen: *LLVMGenerator, ty: HIR.HIRType) LLVMTypes.LLVMTypeRef {
    return switch (ty) {
        .Int => LLVMCore.LLVMInt64TypeInContext(gen.context),
        .Byte => LLVMCore.LLVMInt8TypeInContext(gen.context),
        .Float => LLVMCore.LLVMDoubleTypeInContext(gen.context),
        .String => LLVMCore.LLVMPointerType(LLVMCore.LLVMInt8TypeInContext(gen.context), 0),
        .Tetra => LLVMCore.LLVMIntTypeInContext(gen.context, 2),
        .Nothing => LLVMCore.LLVMVoidTypeInContext(gen.context),
        else => LLVMCore.LLVMInt64TypeInContext(gen.context),
    };
}

fn getOrCreatePuts(gen: *LLVMGenerator) LLVMTypes.LLVMValueRef {
    const i32_ty = LLVMCore.LLVMInt32TypeInContext(gen.context);
    const i8_ptr_ty = LLVMCore.LLVMPointerType(LLVMCore.LLVMInt8TypeInContext(gen.context), 0);
    const existing = LLVMCore.LLVMGetNamedFunction(gen.module, "puts");
    if (existing != null) return existing;
    var params = [_]LLVMTypes.LLVMTypeRef{i8_ptr_ty};
    const puts_ty = LLVMCore.LLVMFunctionType(i32_ty, &params, 1, @intFromBool(false));
    return LLVMCore.LLVMAddFunction(gen.module, "puts", puts_ty);
}

fn getOrCreatePrintf(gen: *LLVMGenerator) LLVMTypes.LLVMValueRef {
    const i32_ty = LLVMCore.LLVMInt32TypeInContext(gen.context);
    const i8_ptr_ty = LLVMCore.LLVMPointerType(LLVMCore.LLVMInt8TypeInContext(gen.context), 0);
    const existing = LLVMCore.LLVMGetNamedFunction(gen.module, "printf");
    if (existing != null) return existing;
    var params = [_]LLVMTypes.LLVMTypeRef{i8_ptr_ty};
    const printf_ty = LLVMCore.LLVMFunctionType(i32_ty, &params, 1, @intFromBool(true));
    return LLVMCore.LLVMAddFunction(gen.module, "printf", printf_ty);
}

fn getOrCreatePow(gen: *LLVMGenerator) LLVMTypes.LLVMValueRef {
    const existing = LLVMCore.LLVMGetNamedFunction(gen.module, "pow");
    if (existing != null) return existing;
    const f64_ty = LLVMCore.LLVMDoubleTypeInContext(gen.context);
    var params = [_]LLVMTypes.LLVMTypeRef{ f64_ty, f64_ty };
    const pow_ty = LLVMCore.LLVMFunctionType(f64_ty, &params, 2, @intFromBool(false));
    return LLVMCore.LLVMAddFunction(gen.module, "pow", pow_ty);
}

fn ensureI64(gen: *LLVMGenerator, v: LLVMTypes.LLVMValueRef) LLVMTypes.LLVMValueRef {
    const ty = LLVMCore.LLVMTypeOf(v);
    if (LLVMCore.LLVMGetTypeKind(ty) == LLVMTypes.LLVMTypeKind.LLVMIntegerTypeKind) {
        const width = LLVMCore.LLVMGetIntTypeWidth(ty);
        if (width == 64) return v;
        if (width < 64) {
            // Treat smaller integers as unsigned for widening (byte semantics)
            return LLVMCore.LLVMBuildZExt(gen.builder, v, LLVMCore.LLVMInt64TypeInContext(gen.context), "zext.i64");
        }
        // Wider than 64 not expected; truncate defensively
        return LLVMCore.LLVMBuildTrunc(gen.builder, v, LLVMCore.LLVMInt64TypeInContext(gen.context), "trunc.i64");
    }
    if (LLVMCore.LLVMGetTypeKind(ty) == LLVMTypes.LLVMTypeKind.LLVMDoubleTypeKind) {
        return LLVMCore.LLVMBuildFPToSI(gen.builder, v, LLVMCore.LLVMInt64TypeInContext(gen.context), "fp2si.i64");
    }
    return v;
}

fn ensureF64(gen: *LLVMGenerator, v: LLVMTypes.LLVMValueRef) LLVMTypes.LLVMValueRef {
    const ty = LLVMCore.LLVMTypeOf(v);
    const kind = LLVMCore.LLVMGetTypeKind(ty);
    if (kind == LLVMTypes.LLVMTypeKind.LLVMDoubleTypeKind) return v;
    if (kind == LLVMTypes.LLVMTypeKind.LLVMFloatTypeKind) {
        return LLVMCore.LLVMBuildFPExt(gen.builder, v, LLVMCore.LLVMDoubleTypeInContext(gen.context), "fpext.f64");
    }
    if (kind == LLVMTypes.LLVMTypeKind.LLVMIntegerTypeKind) {
        return LLVMCore.LLVMBuildSIToFP(gen.builder, v, LLVMCore.LLVMDoubleTypeInContext(gen.context), "si2fp.f64");
    }
    return v;
}

fn ensureI8(gen: *LLVMGenerator, v: LLVMTypes.LLVMValueRef) LLVMTypes.LLVMValueRef {
    const ty = LLVMCore.LLVMTypeOf(v);
    if (LLVMCore.LLVMGetTypeKind(ty) == LLVMTypes.LLVMTypeKind.LLVMIntegerTypeKind and LLVMCore.LLVMGetIntTypeWidth(ty) == 8) {
        return v;
    }
    const i8_ty = LLVMCore.LLVMInt8TypeInContext(gen.context);
    const kind = LLVMCore.LLVMGetTypeKind(ty);
    if (kind == LLVMTypes.LLVMTypeKind.LLVMIntegerTypeKind) {
        return LLVMCore.LLVMBuildTrunc(gen.builder, v, i8_ty, "trunc.i8");
    }
    if (kind == LLVMTypes.LLVMTypeKind.LLVMDoubleTypeKind or kind == LLVMTypes.LLVMTypeKind.LLVMFloatTypeKind) {
        return LLVMCore.LLVMBuildFPToUI(gen.builder, v, i8_ty, "fp2ui.i8");
    }
    return v;
}

fn byteSaturatingAdd(gen: *LLVMGenerator, a: LLVMTypes.LLVMValueRef, b: LLVMTypes.LLVMValueRef) LLVMTypes.LLVMValueRef {
    const i8_ty = LLVMCore.LLVMInt8TypeInContext(gen.context);
    const i16_ty = LLVMCore.LLVMInt16TypeInContext(gen.context);
    const a16 = LLVMCore.LLVMBuildZExt(gen.builder, ensureI8(gen, a), i16_ty, "a.z16");
    const b16 = LLVMCore.LLVMBuildZExt(gen.builder, ensureI8(gen, b), i16_ty, "b.z16");
    const sum16 = LLVMCore.LLVMBuildAdd(gen.builder, a16, b16, "add16");
    const max255 = LLVMCore.LLVMConstInt(i16_ty, 255, @intFromBool(false));
    const gt = LLVMCore.LLVMBuildICmp(gen.builder, LLVMTypes.LLVMIntPredicate.LLVMIntUGT, sum16, max255, "gt255");
    const clamped = LLVMCore.LLVMBuildSelect(gen.builder, gt, max255, sum16, "sel");
    return LLVMCore.LLVMBuildTrunc(gen.builder, clamped, i8_ty, "to.i8");
}

fn byteSaturatingSub(gen: *LLVMGenerator, a: LLVMTypes.LLVMValueRef, b: LLVMTypes.LLVMValueRef) LLVMTypes.LLVMValueRef {
    const i8_ty = LLVMCore.LLVMInt8TypeInContext(gen.context);
    const a8 = ensureI8(gen, a);
    const b8 = ensureI8(gen, b);
    const ge = LLVMCore.LLVMBuildICmp(gen.builder, LLVMTypes.LLVMIntPredicate.LLVMIntUGE, a8, b8, "ge");
    const diff = LLVMCore.LLVMBuildSub(gen.builder, a8, b8, "sub8");
    const zero = LLVMCore.LLVMConstInt(i8_ty, 0, @intFromBool(false));
    return LLVMCore.LLVMBuildSelect(gen.builder, ge, diff, zero, "sel");
}

fn byteSaturatingMul(gen: *LLVMGenerator, a: LLVMTypes.LLVMValueRef, b: LLVMTypes.LLVMValueRef) LLVMTypes.LLVMValueRef {
    const i8_ty = LLVMCore.LLVMInt8TypeInContext(gen.context);
    const i32_ty = LLVMCore.LLVMInt32TypeInContext(gen.context);
    const a32 = LLVMCore.LLVMBuildZExt(gen.builder, ensureI8(gen, a), i32_ty, "a.z32");
    const b32 = LLVMCore.LLVMBuildZExt(gen.builder, ensureI8(gen, b), i32_ty, "b.z32");
    const prod32 = LLVMCore.LLVMBuildMul(gen.builder, a32, b32, "mul32");
    const max255 = LLVMCore.LLVMConstInt(i32_ty, 255, @intFromBool(false));
    const gt = LLVMCore.LLVMBuildICmp(gen.builder, LLVMTypes.LLVMIntPredicate.LLVMIntUGT, prod32, max255, "gt255");
    const sel = LLVMCore.LLVMBuildSelect(gen.builder, gt, max255, prod32, "sel");
    const trunc = LLVMCore.LLVMBuildTrunc(gen.builder, sel, i8_ty, "to.i8");
    return trunc;
}

fn createStringPtr(gen: *LLVMGenerator, s: []const u8) !LLVMTypes.LLVMValueRef {
    // Create a null-terminated copy to satisfy [*:0]const u8
    var buf = try gen.allocator.alloc(u8, s.len + 1);
    defer gen.allocator.free(buf);
    @memcpy(buf[0..s.len], s);
    buf[s.len] = 0;

    const str_val = LLVMCore.LLVMConstStringInContext(gen.context, @ptrCast(buf.ptr), @intCast(buf.len), @intFromBool(false));

    // Generate a unique name per call to avoid collisions between multiple literals
    var name_buf: [32]u8 = undefined;
    const id = next_string_id;
    next_string_id += 1;
    const zname = std.fmt.bufPrintZ(&name_buf, "str.lit.{d}", .{id}) catch "str.lit";

    const global = LLVMCore.LLVMAddGlobal(gen.module, LLVMCore.LLVMTypeOf(str_val), zname.ptr);
    LLVMCore.LLVMSetInitializer(global, str_val);
    LLVMCore.LLVMSetGlobalConstant(global, @intFromBool(true));
    LLVMCore.LLVMSetLinkage(global, LLVMTypes.LLVMLinkage.LLVMPrivateLinkage);
    const i32_ty = LLVMCore.LLVMInt32TypeInContext(gen.context);
    const zero = LLVMCore.LLVMConstInt(i32_ty, 0, @intFromBool(false));
    var idx = [_]LLVMTypes.LLVMValueRef{ zero, zero };
    const gep = LLVMCore.LLVMBuildGEP2(gen.builder, LLVMCore.LLVMTypeOf(str_val), global, &idx, 2, "str.gep");
    const i8_ptr_ty = LLVMCore.LLVMPointerType(LLVMCore.LLVMInt8TypeInContext(gen.context), 0);
    return LLVMCore.LLVMBuildBitCast(gen.builder, gep, i8_ptr_ty, "str.cast");
}

fn ensureI1(gen: *LLVMGenerator, v: LLVMTypes.LLVMValueRef) LLVMTypes.LLVMValueRef {
    const ty = LLVMCore.LLVMTypeOf(v);
    const kind = LLVMCore.LLVMGetTypeKind(ty);
    const i1_ty = LLVMCore.LLVMInt1TypeInContext(gen.context);
    switch (kind) {
        LLVMTypes.LLVMTypeKind.LLVMIntegerTypeKind => {
            const width = LLVMCore.LLVMGetIntTypeWidth(ty);
            if (width == 1) return v;
            const zero = LLVMCore.LLVMConstInt(ty, 0, @intFromBool(false));
            return LLVMCore.LLVMBuildICmp(gen.builder, LLVMTypes.LLVMIntPredicate.LLVMIntNE, v, zero, "to.i1");
        },
        LLVMTypes.LLVMTypeKind.LLVMDoubleTypeKind, LLVMTypes.LLVMTypeKind.LLVMFloatTypeKind => {
            const f = ensureF64(gen, v);
            const zero = LLVMCore.LLVMConstReal(LLVMCore.LLVMDoubleTypeInContext(gen.context), 0);
            return LLVMCore.LLVMBuildFCmp(gen.builder, LLVMTypes.LLVMRealPredicate.LLVMRealONE, f, zero, "to.i1");
        },
        LLVMTypes.LLVMTypeKind.LLVMPointerTypeKind => {
            const null_ptr = LLVMCore.LLVMConstNull(ty);
            return LLVMCore.LLVMBuildICmp(gen.builder, LLVMTypes.LLVMIntPredicate.LLVMIntNE, v, null_ptr, "to.i1");
        },
        else => return LLVMCore.LLVMConstInt(i1_ty, 0, @intFromBool(false)),
    }
}

pub fn translateToLLVM(hir: *const HIR.HIRProgram, generator: *LLVMGenerator) !void {
    const i32_ty = LLVMCore.LLVMInt32TypeInContext(generator.context);
    const fn_ty = LLVMCore.LLVMFunctionType(i32_ty, null, 0, @intFromBool(false));
    const main_fn = LLVMCore.LLVMAddFunction(generator.module, "main", fn_ty);
    const entry = LLVMCore.LLVMAppendBasicBlockInContext(generator.context, main_fn, "entry");
    LLVMCore.LLVMPositionBuilderAtEnd(generator.builder, entry);

    var stack = std.array_list.Managed(LLVMTypes.LLVMValueRef).init(generator.allocator);
    defer stack.deinit();

    var block_map = std.StringHashMap(LLVMTypes.LLVMBasicBlockRef).init(generator.allocator);
    defer block_map.deinit();

    const VarPtrEntry = struct {
        ptr: LLVMTypes.LLVMValueRef,
        elem_ty: LLVMTypes.LLVMTypeRef,
    };
    var var_ptr_map = std.StringHashMap(VarPtrEntry).init(generator.allocator);
    defer var_ptr_map.deinit();

    for (hir.instructions) |inst| {
        switch (inst) {
            .Const => |c| {
                const idx = c.constant_id;
                if (idx >= hir.constant_pool.len) continue;
                const hv = hir.constant_pool[idx];
                switch (hv) {
                    .string => |s| {
                        const ptr = try createStringPtr(generator, s);
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
                        const lf = ensureF64(generator, left);
                        const rf = ensureF64(generator, right);
                        result = switch (a.op) {
                            .Add => LLVMCore.LLVMBuildFAdd(generator.builder, lf, rf, "fadd"),
                            .Sub => LLVMCore.LLVMBuildFSub(generator.builder, lf, rf, "fsub"),
                            .Mul => LLVMCore.LLVMBuildFMul(generator.builder, lf, rf, "fmul"),
                            .Div => LLVMCore.LLVMBuildFDiv(generator.builder, lf, rf, "fdiv"),
                            .Mod => LLVMCore.LLVMBuildFRem(generator.builder, lf, rf, "frem"),
                            .Pow => blk: {
                                const pow_fn = getOrCreatePow(generator);
                                var args = [_]LLVMTypes.LLVMValueRef{ lf, rf };
                                const f64_ty = LLVMCore.LLVMDoubleTypeInContext(generator.context);
                                var ptys = [_]LLVMTypes.LLVMTypeRef{ f64_ty, f64_ty };
                                const pty = LLVMCore.LLVMFunctionType(f64_ty, &ptys, 2, @intFromBool(false));
                                break :blk LLVMCore.LLVMBuildCall2(generator.builder, pty, pow_fn, &args, 2, "pow");
                            },
                        };
                    },
                    .Int => {
                        const li64 = ensureI64(generator, left);
                        const ri64 = ensureI64(generator, right);
                        result = switch (a.op) {
                            .Add => LLVMCore.LLVMBuildAdd(generator.builder, li64, ri64, "add"),
                            .Sub => LLVMCore.LLVMBuildSub(generator.builder, li64, ri64, "sub"),
                            .Mul => LLVMCore.LLVMBuildMul(generator.builder, li64, ri64, "mul"),
                            .Div => blk_div: {
                                const lf = ensureF64(generator, li64);
                                const rf = ensureF64(generator, ri64);
                                break :blk_div LLVMCore.LLVMBuildFDiv(generator.builder, lf, rf, "idiv.f");
                            },
                            .Mod => LLVMCore.LLVMBuildSRem(generator.builder, li64, ri64, "srem"),
                            .Pow => blk_pow: {
                                const lf = ensureF64(generator, li64);
                                const rf = ensureF64(generator, ri64);
                                const pow_fn = getOrCreatePow(generator);
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
                                const a8 = ensureI8(generator, left);
                                const b8 = ensureI8(generator, right);
                                const zero8 = LLVMCore.LLVMConstInt(LLVMCore.LLVMInt8TypeInContext(generator.context), 0, @intFromBool(false));
                                const is_zero = LLVMCore.LLVMBuildICmp(generator.builder, LLVMTypes.LLVMIntPredicate.LLVMIntEQ, b8, zero8, "is0");
                                const quot = LLVMCore.LLVMBuildUDiv(generator.builder, a8, b8, "udiv8");
                                break :blk_bdiv LLVMCore.LLVMBuildSelect(generator.builder, is_zero, zero8, quot, "sel");
                            },
                            .Mod => blk_bmod: {
                                const a8 = ensureI8(generator, left);
                                const b8 = ensureI8(generator, right);
                                const zero8 = LLVMCore.LLVMConstInt(LLVMCore.LLVMInt8TypeInContext(generator.context), 0, @intFromBool(false));
                                const is_zero = LLVMCore.LLVMBuildICmp(generator.builder, LLVMTypes.LLVMIntPredicate.LLVMIntEQ, b8, zero8, "is0");
                                const rem = LLVMCore.LLVMBuildURem(generator.builder, a8, b8, "urem8");
                                break :blk_bmod LLVMCore.LLVMBuildSelect(generator.builder, is_zero, zero8, rem, "sel");
                            },
                            .Pow => blk_bpow: {
                                const f64_ty = LLVMCore.LLVMDoubleTypeInContext(generator.context);
                                const a_f = ensureF64(generator, ensureI8(generator, left));
                                const b_f = ensureF64(generator, ensureI8(generator, right));
                                const pow_fn = getOrCreatePow(generator);
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
                                out = ensureI64(generator, v);
                            },
                            .Byte => {
                                const i64_ty = LLVMCore.LLVMInt64TypeInContext(generator.context);
                                out = LLVMCore.LLVMBuildZExt(generator.builder, ensureI8(generator, v), i64_ty, "zext.i64");
                            },
                            .Float => {
                                const i64_ty = LLVMCore.LLVMInt64TypeInContext(generator.context);
                                out = LLVMCore.LLVMBuildFPToSI(generator.builder, ensureF64(generator, v), i64_ty, "fp2si.i64");
                            },
                            else => {},
                        }
                    },
                    .Byte => {
                        const i8_ty = LLVMCore.LLVMInt8TypeInContext(generator.context);
                        switch (conv.from_type) {
                            .Int => {
                                out = LLVMCore.LLVMBuildTrunc(generator.builder, ensureI64(generator, v), i8_ty, "trunc.i8");
                            },
                            .Byte => {
                                out = ensureI8(generator, v);
                            },
                            .Float => {
                                out = LLVMCore.LLVMBuildFPToUI(generator.builder, ensureF64(generator, v), i8_ty, "fp2ui.i8");
                            },
                            else => {},
                        }
                    },
                    .Float => {
                        const f64_ty = LLVMCore.LLVMDoubleTypeInContext(generator.context);
                        switch (conv.from_type) {
                            .Int => {
                                out = LLVMCore.LLVMBuildSIToFP(generator.builder, ensureI64(generator, v), f64_ty, "si2fp.f64");
                            },
                            .Byte => {
                                const i8v = ensureI8(generator, v);
                                out = LLVMCore.LLVMBuildUIToFP(generator.builder, i8v, f64_ty, "ui2fp.f64");
                            },
                            .Float => {
                                out = ensureF64(generator, v);
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
                        const li = ensureI64(generator, left);
                        const ri = ensureI64(generator, right);
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
                        const li8 = ensureI8(generator, left);
                        const ri8 = ensureI8(generator, right);
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
                        const lf = ensureF64(generator, left);
                        const rf = ensureF64(generator, right);
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
                    const ty = LLVMCore.LLVMTypeOf(v);
                    const kind = LLVMCore.LLVMGetTypeKind(ty);
                    const printf_fn = getOrCreatePrintf(generator);
                    const i8_ptr_ty = LLVMCore.LLVMPointerType(LLVMCore.LLVMInt8TypeInContext(generator.context), 0);
                    var printf_param_types = [_]LLVMTypes.LLVMTypeRef{i8_ptr_ty};
                    const printf_ty = LLVMCore.LLVMFunctionType(i32_ty, &printf_param_types, 1, @intFromBool(true));

                    if (kind == LLVMTypes.LLVMTypeKind.LLVMPointerTypeKind) {
                        const fmt = try createStringPtr(generator, "%s");
                        var args = [_]LLVMTypes.LLVMValueRef{ fmt, v };
                        _ = LLVMCore.LLVMBuildCall2(generator.builder, printf_ty, printf_fn, &args, 2, "printf");
                    } else if (kind == LLVMTypes.LLVMTypeKind.LLVMDoubleTypeKind or kind == LLVMTypes.LLVMTypeKind.LLVMFloatTypeKind) {
                        const fmt = try createStringPtr(generator, "%g");
                        const vf = ensureF64(generator, v);
                        var args = [_]LLVMTypes.LLVMValueRef{ fmt, vf };
                        _ = LLVMCore.LLVMBuildCall2(generator.builder, printf_ty, printf_fn, &args, 2, "printf");
                    } else if (kind == LLVMTypes.LLVMTypeKind.LLVMIntegerTypeKind) {
                        const width = LLVMCore.LLVMGetIntTypeWidth(ty);
                        if (width == 1) {
                            const fmt = try createStringPtr(generator, "%d");
                            const i32_ty2 = LLVMCore.LLVMInt32TypeInContext(generator.context);
                            const v32 = LLVMCore.LLVMBuildZExt(generator.builder, v, i32_ty2, "zext.i32");
                            var args = [_]LLVMTypes.LLVMValueRef{ fmt, v32 };
                            _ = LLVMCore.LLVMBuildCall2(generator.builder, printf_ty, printf_fn, &args, 2, "printf");
                        } else if (width == 8) {
                            // Print byte as unsigned
                            const fmt = try createStringPtr(generator, "%u");
                            const i32_ty2 = LLVMCore.LLVMInt32TypeInContext(generator.context);
                            const v32 = LLVMCore.LLVMBuildZExt(generator.builder, v, i32_ty2, "zext.i32");
                            var args = [_]LLVMTypes.LLVMValueRef{ fmt, v32 };
                            _ = LLVMCore.LLVMBuildCall2(generator.builder, printf_ty, printf_fn, &args, 2, "printf");
                        } else {
                            const fmt = try createStringPtr(generator, "%ld");
                            const v64 = LLVMCore.LLVMBuildSExt(generator.builder, v, LLVMCore.LLVMInt64TypeInContext(generator.context), "sext.i64");
                            var args = [_]LLVMTypes.LLVMValueRef{ fmt, v64 };
                            _ = LLVMCore.LLVMBuildCall2(generator.builder, printf_ty, printf_fn, &args, 2, "printf");
                        }
                    } else {
                        // Fallback: do nothing
                    }
                }
            },
            .PrintBegin => {},
            .PrintStr => |p| {
                if (p.const_id < hir.constant_pool.len) {
                    const hv = hir.constant_pool[p.const_id];
                    if (hv == .string) {
                        const str_ptr = try createStringPtr(generator, hv.string);
                        const printf_fn = getOrCreatePrintf(generator);
                        const fmt = try createStringPtr(generator, "%s");
                        var args = [_]LLVMTypes.LLVMValueRef{ fmt, str_ptr };
                        const i8_ptr_ty = LLVMCore.LLVMPointerType(LLVMCore.LLVMInt8TypeInContext(generator.context), 0);
                        var printf_param_types = [_]LLVMTypes.LLVMTypeRef{i8_ptr_ty};
                        const printf_ty = LLVMCore.LLVMFunctionType(i32_ty, &printf_param_types, 1, @intFromBool(true));
                        _ = LLVMCore.LLVMBuildCall2(generator.builder, printf_ty, printf_fn, &args, 2, "printf");
                    }
                }
            },
            .PrintVal => {
                if (stack.items.len > 0) {
                    const v = stack.items[stack.items.len - 1];
                    stack.items.len -= 1;
                    const ty = LLVMCore.LLVMTypeOf(v);
                    const kind = LLVMCore.LLVMGetTypeKind(ty);
                    const printf_fn = getOrCreatePrintf(generator);
                    const i8_ptr_ty = LLVMCore.LLVMPointerType(LLVMCore.LLVMInt8TypeInContext(generator.context), 0);
                    var printf_param_types = [_]LLVMTypes.LLVMTypeRef{i8_ptr_ty};
                    const printf_ty = LLVMCore.LLVMFunctionType(i32_ty, &printf_param_types, 1, @intFromBool(true));

                    if (kind == LLVMTypes.LLVMTypeKind.LLVMPointerTypeKind) {
                        const fmt = try createStringPtr(generator, "%s");
                        var args = [_]LLVMTypes.LLVMValueRef{ fmt, v };
                        _ = LLVMCore.LLVMBuildCall2(generator.builder, printf_ty, printf_fn, &args, 2, "printf");
                    } else if (kind == LLVMTypes.LLVMTypeKind.LLVMDoubleTypeKind or kind == LLVMTypes.LLVMTypeKind.LLVMFloatTypeKind) {
                        const fmt = try createStringPtr(generator, "%g");
                        const vf = ensureF64(generator, v);
                        var args = [_]LLVMTypes.LLVMValueRef{ fmt, vf };
                        _ = LLVMCore.LLVMBuildCall2(generator.builder, printf_ty, printf_fn, &args, 2, "printf");
                    } else if (kind == LLVMTypes.LLVMTypeKind.LLVMIntegerTypeKind) {
                        const width = LLVMCore.LLVMGetIntTypeWidth(ty);
                        if (width == 1) {
                            const fmt = try createStringPtr(generator, "%d");
                            const i32_ty2 = LLVMCore.LLVMInt32TypeInContext(generator.context);
                            const v32 = LLVMCore.LLVMBuildZExt(generator.builder, v, i32_ty2, "zext.i32");
                            var args = [_]LLVMTypes.LLVMValueRef{ fmt, v32 };
                            _ = LLVMCore.LLVMBuildCall2(generator.builder, printf_ty, printf_fn, &args, 2, "printf");
                        } else if (width == 8) {
                            const fmt = try createStringPtr(generator, "%u");
                            const i32_ty2 = LLVMCore.LLVMInt32TypeInContext(generator.context);
                            const v32 = LLVMCore.LLVMBuildZExt(generator.builder, v, i32_ty2, "zext.i32");
                            var args = [_]LLVMTypes.LLVMValueRef{ fmt, v32 };
                            _ = LLVMCore.LLVMBuildCall2(generator.builder, printf_ty, printf_fn, &args, 2, "printf");
                        } else if (width == 2) {
                            // Print tetra as integer 0..3 for simplicity
                            const fmt = try createStringPtr(generator, "%ld");
                            const v64 = LLVMCore.LLVMBuildZExt(generator.builder, v, LLVMCore.LLVMInt64TypeInContext(generator.context), "zext.i64");
                            var args = [_]LLVMTypes.LLVMValueRef{ fmt, v64 };
                            _ = LLVMCore.LLVMBuildCall2(generator.builder, printf_ty, printf_fn, &args, 2, "printf");
                        } else {
                            const fmt = try createStringPtr(generator, "%ld");
                            const v64 = LLVMCore.LLVMBuildSExt(generator.builder, v, LLVMCore.LLVMInt64TypeInContext(generator.context), "sext.i64");
                            var args = [_]LLVMTypes.LLVMValueRef{ fmt, v64 };
                            _ = LLVMCore.LLVMBuildCall2(generator.builder, printf_ty, printf_fn, &args, 2, "printf");
                        }
                    } else {
                        // Fallback: do nothing
                    }
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
                    break;
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

                    // Print prefix
                    const printf_fn = getOrCreatePrintf(generator);
                    var args0 = [_]LLVMTypes.LLVMValueRef{try createStringPtr(generator, prefix_slice)};
                    const i8_ptr_ty = LLVMCore.LLVMPointerType(LLVMCore.LLVMInt8TypeInContext(generator.context), 0);
                    var printf_param_types = [_]LLVMTypes.LLVMTypeRef{i8_ptr_ty};
                    const printf_ty = LLVMCore.LLVMFunctionType(i32_ty, &printf_param_types, 1, @intFromBool(true));
                    _ = LLVMCore.LLVMBuildCall2(generator.builder, printf_ty, printf_fn, &args0, 1, "printf");

                    // Then print the value according to kind
                    if (kind == LLVMTypes.LLVMTypeKind.LLVMPointerTypeKind) {
                        var args = [_]LLVMTypes.LLVMValueRef{ try createStringPtr(generator, "%s"), v };
                        _ = LLVMCore.LLVMBuildCall2(generator.builder, printf_ty, printf_fn, &args, 2, "printf");
                    } else if (kind == LLVMTypes.LLVMTypeKind.LLVMDoubleTypeKind or kind == LLVMTypes.LLVMTypeKind.LLVMFloatTypeKind) {
                        const vf = ensureF64(generator, v);
                        var args = [_]LLVMTypes.LLVMValueRef{ try createStringPtr(generator, "%g"), vf };
                        _ = LLVMCore.LLVMBuildCall2(generator.builder, printf_ty, printf_fn, &args, 2, "printf");
                    } else if (kind == LLVMTypes.LLVMTypeKind.LLVMIntegerTypeKind) {
                        const vi = ensureI64(generator, v);
                        var args = [_]LLVMTypes.LLVMValueRef{ try createStringPtr(generator, "%ld"), vi };
                        _ = LLVMCore.LLVMBuildCall2(generator.builder, printf_ty, printf_fn, &args, 2, "printf");
                    } else {
                        // Fallback do nothing
                    }

                    // Trailing newline
                    var args_nl = [_]LLVMTypes.LLVMValueRef{try createStringPtr(generator, "\n")};
                    _ = LLVMCore.LLVMBuildCall2(generator.builder, printf_ty, printf_fn, &args_nl, 1, "printf");

                    // Push original value back on the stack
                    try stack.append(v);
                }
            },
            .PrintNewline => {
                const printf_fn = getOrCreatePrintf(generator);
                const nl = try createStringPtr(generator, "\n");
                var args = [_]LLVMTypes.LLVMValueRef{nl};
                const i8_ptr_ty = LLVMCore.LLVMPointerType(LLVMCore.LLVMInt8TypeInContext(generator.context), 0);
                var printf_param_types = [_]LLVMTypes.LLVMTypeRef{i8_ptr_ty};
                const printf_ty = LLVMCore.LLVMFunctionType(i32_ty, &printf_param_types, 1, @intFromBool(true));
                _ = LLVMCore.LLVMBuildCall2(generator.builder, printf_ty, printf_fn, &args, 1, "printf");
            },
            .PrintEnd => {},
            .Halt => {
                const zero_ret = LLVMCore.LLVMConstInt(i32_ty, 0, @intFromBool(false));
                _ = LLVMCore.LLVMBuildRet(generator.builder, zero_ret);
                break;
            },
            .Label => |lbl| {
                const insert_block = LLVMCore.LLVMGetInsertBlock(generator.builder);
                var need_branch: bool = false;
                if (insert_block != null and LLVMCore.LLVMGetBasicBlockTerminator(insert_block) == null) {
                    need_branch = true;
                }

                const bb_opt = block_map.get(lbl.name);
                var bb: LLVMTypes.LLVMBasicBlockRef = undefined;
                if (bb_opt) |existing| {
                    bb = existing;
                } else {
                    const zname = try generator.allocator.dupeZ(u8, lbl.name);
                    bb = LLVMCore.LLVMAppendBasicBlockInContext(generator.context, main_fn, zname.ptr);
                    try block_map.put(lbl.name, bb);
                }

                if (need_branch) {
                    _ = LLVMCore.LLVMBuildBr(generator.builder, bb);
                }
                LLVMCore.LLVMPositionBuilderAtEnd(generator.builder, bb);
            },
            .Jump => |j| {
                const target_bb_opt = block_map.get(j.label);
                var target_bb: LLVMTypes.LLVMBasicBlockRef = undefined;
                if (target_bb_opt) |existing| {
                    target_bb = existing;
                } else {
                    const zname = try generator.allocator.dupeZ(u8, j.label);
                    target_bb = LLVMCore.LLVMAppendBasicBlockInContext(generator.context, main_fn, zname.ptr);
                    try block_map.put(j.label, target_bb);
                }
                _ = LLVMCore.LLVMBuildBr(generator.builder, target_bb);
                var name_buf: [32]u8 = undefined;
                const cont_z = std.fmt.bufPrintZ(&name_buf, "cont.{d}", .{next_block_id}) catch "cont";
                next_block_id += 1;
                const cont_bb = LLVMCore.LLVMAppendBasicBlockInContext(generator.context, main_fn, cont_z.ptr);
                LLVMCore.LLVMPositionBuilderAtEnd(generator.builder, cont_bb);
            },
            .JumpCond => |jc| {
                if (stack.items.len < 1) continue;
                const cond_v = stack.items[stack.items.len - 1];
                stack.items.len -= 1;
                const cond_i1 = ensureI1(generator, cond_v);

                const true_bb = block_map.get(jc.label_true) orelse blk: {
                    const zname = try generator.allocator.dupeZ(u8, jc.label_true);
                    const bb = LLVMCore.LLVMAppendBasicBlockInContext(generator.context, main_fn, zname.ptr);
                    try block_map.put(jc.label_true, bb);
                    break :blk bb;
                };
                const false_bb = block_map.get(jc.label_false) orelse blk2: {
                    const zname = try generator.allocator.dupeZ(u8, jc.label_false);
                    const bb = LLVMCore.LLVMAppendBasicBlockInContext(generator.context, main_fn, zname.ptr);
                    try block_map.put(jc.label_false, bb);
                    break :blk2 bb;
                };

                _ = LLVMCore.LLVMBuildCondBr(generator.builder, cond_i1, true_bb, false_bb);

                var name_buf: [32]u8 = undefined;
                const cont_z = std.fmt.bufPrintZ(&name_buf, "cont.{d}", .{next_block_id}) catch "cont";
                next_block_id += 1;
                const cont_bb = LLVMCore.LLVMAppendBasicBlockInContext(generator.context, main_fn, cont_z.ptr);
                LLVMCore.LLVMPositionBuilderAtEnd(generator.builder, cont_bb);
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

                const target_ty = mapHIRTypeToLLVM(generator, sv.expected_type);
                const target_kind = LLVMCore.LLVMGetTypeKind(target_ty);
                // Coerce v to target_ty for basic numeric types
                if (target_kind == LLVMTypes.LLVMTypeKind.LLVMIntegerTypeKind) {
                    const w = LLVMCore.LLVMGetIntTypeWidth(target_ty);
                    if (w == 64) v = ensureI64(generator, v) else if (w == 8) v = ensureI8(generator, v) else if (w == 1) v = ensureI1(generator, v) else {}
                } else if (target_kind == LLVMTypes.LLVMTypeKind.LLVMDoubleTypeKind) {
                    v = ensureF64(generator, v);
                }

                if (sv.scope_kind == HIR.ScopeKind.Local) {
                    const ptr_opt = var_ptr_map.get(key);
                    const ptr_entry: VarPtrEntry = blk_ptr: {
                        if (ptr_opt) |existing| {
                            break :blk_ptr existing;
                        } else {
                            const curr_bb = LLVMCore.LLVMGetInsertBlock(generator.builder);
                            LLVMCore.LLVMPositionBuilderAtEnd(generator.builder, entry);
                            const new_ptr = LLVMCore.LLVMBuildAlloca(generator.builder, target_ty, (try generator.allocator.dupeZ(u8, sv.var_name)).ptr);
                            LLVMCore.LLVMPositionBuilderAtEnd(generator.builder, curr_bb);
                            try var_ptr_map.put(key, .{ .ptr = new_ptr, .elem_ty = target_ty });
                            break :blk_ptr .{ .ptr = new_ptr, .elem_ty = target_ty };
                        }
                    };
                    _ = LLVMCore.LLVMBuildStore(generator.builder, v, ptr_entry.ptr);
                } else {
                    const gptr_opt = var_ptr_map.get(key);
                    const gptr_entry: VarPtrEntry = blk_g: {
                        if (gptr_opt) |existing| {
                            break :blk_g existing;
                        } else {
                            const zsym = try generator.allocator.dupeZ(u8, key);
                            const new_g = LLVMCore.LLVMAddGlobal(generator.module, target_ty, zsym.ptr);
                            LLVMCore.LLVMSetLinkage(new_g, LLVMTypes.LLVMLinkage.LLVMExternalLinkage);
                            _ = LLVMCore.LLVMSetInitializer(new_g, LLVMCore.LLVMConstNull(target_ty));
                            try var_ptr_map.put(key, .{ .ptr = new_g, .elem_ty = target_ty });
                            break :blk_g .{ .ptr = new_g, .elem_ty = target_ty };
                        }
                    };
                    _ = LLVMCore.LLVMBuildStore(generator.builder, v, gptr_entry.ptr);
                }
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

                if (var_ptr_map.get(key)) |vp_entry| {
                    const loaded = LLVMCore.LLVMBuildLoad2(generator.builder, vp_entry.elem_ty, vp_entry.ptr, "load");
                    try stack.append(loaded);
                } else {
                    // Create an implicit i64 local initialized to 0
                    const target_ty = LLVMCore.LLVMInt64TypeInContext(generator.context);
                    const curr_bb = LLVMCore.LLVMGetInsertBlock(generator.builder);
                    LLVMCore.LLVMPositionBuilderAtEnd(generator.builder, entry);
                    const ptr = LLVMCore.LLVMBuildAlloca(generator.builder, target_ty, (try generator.allocator.dupeZ(u8, lv.var_name)).ptr);
                    _ = LLVMCore.LLVMBuildStore(generator.builder, LLVMCore.LLVMConstInt(target_ty, 0, @intFromBool(false)), ptr);
                    LLVMCore.LLVMPositionBuilderAtEnd(generator.builder, curr_bb);
                    try var_ptr_map.put(key, .{ .ptr = ptr, .elem_ty = target_ty });
                    const loaded = LLVMCore.LLVMBuildLoad2(generator.builder, target_ty, ptr, "load");
                    try stack.append(loaded);
                }
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

                const val_ty = LLVMCore.LLVMTypeOf(v);
                const zsym = try generator.allocator.dupeZ(u8, key);
                const g = LLVMCore.LLVMAddGlobal(generator.module, val_ty, zsym.ptr);
                LLVMCore.LLVMSetLinkage(g, LLVMTypes.LLVMLinkage.LLVMExternalLinkage);
                if (LLVMCore.LLVMIsConstant(v) != 0) {
                    // Compile-time constant: use as initializer and mark global constant
                    LLVMCore.LLVMSetInitializer(g, v);
                    LLVMCore.LLVMSetGlobalConstant(g, @intFromBool(true));
                } else {
                    // Not a compile-time constant: initialize to zero and store at runtime
                    _ = LLVMCore.LLVMSetInitializer(g, LLVMCore.LLVMConstNull(val_ty));
                    _ = LLVMCore.LLVMBuildStore(generator.builder, v, g);
                }
                try var_ptr_map.put(key, .{ .ptr = g, .elem_ty = val_ty });
            },
            else => {},
        }
    }

    const insert_block = LLVMCore.LLVMGetInsertBlock(generator.builder);
    if (insert_block != null and LLVMCore.LLVMGetBasicBlockTerminator(insert_block) == null) {
        const zero_ret = LLVMCore.LLVMConstInt(i32_ty, 0, @intFromBool(false));
        _ = LLVMCore.LLVMBuildRet(generator.builder, zero_ret);
    }
}
