const std = @import("std");
const llvm = @import("llvm");
const LLVMCore = llvm.core;
const LLVMTypes = llvm.types;

const HIR = @import("../hir/soxa_types.zig");
const HIRValue = @import("../hir/soxa_values.zig").HIRValue;
const HIRInstruction = @import("../hir/soxa_instructions.zig").HIRInstruction;
const LLVMGenerator = @import("llvm.zig").LLVMGenerator;

var next_string_id: usize = 0; // unique IDs for string globals within this module

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

pub fn translateToLLVM(hir: *const HIR.HIRProgram, generator: *LLVMGenerator) !void {
    const i32_ty = LLVMCore.LLVMInt32TypeInContext(generator.context);
    const fn_ty = LLVMCore.LLVMFunctionType(i32_ty, null, 0, @intFromBool(false));
    const main_fn = LLVMCore.LLVMAddFunction(generator.module, "main", fn_ty);
    const entry = LLVMCore.LLVMAppendBasicBlockInContext(generator.context, main_fn, "entry");
    LLVMCore.LLVMPositionBuilderAtEnd(generator.builder, entry);

    var stack = std.array_list.Managed(LLVMTypes.LLVMValueRef).init(generator.allocator);
    defer stack.deinit();

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
                        if (width == 8) {
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
                        if (width == 8) {
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
            else => {},
        }
    }

    const insert_block = LLVMCore.LLVMGetInsertBlock(generator.builder);
    if (insert_block != null and LLVMCore.LLVMGetBasicBlockTerminator(insert_block) == null) {
        const zero_ret = LLVMCore.LLVMConstInt(i32_ty, 0, @intFromBool(false));
        _ = LLVMCore.LLVMBuildRet(generator.builder, zero_ret);
    }
}
