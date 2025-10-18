const llvm = @import("llvm");
const LLVMCore = llvm.core;
const LLVMTypes = llvm.types;
const LLVMGenerator = @import("../llvm.zig").LLVMGenerator;

pub fn ensureI64(gen: *LLVMGenerator, v: LLVMTypes.LLVMValueRef) LLVMTypes.LLVMValueRef {
    const ty = LLVMCore.LLVMTypeOf(v);
    if (LLVMCore.LLVMGetTypeKind(ty) == LLVMTypes.LLVMTypeKind.LLVMIntegerTypeKind) {
        const width = LLVMCore.LLVMGetIntTypeWidth(ty);
        if (width == 64) return v;
        if (width < 64) {
            return LLVMCore.LLVMBuildZExt(gen.builder, v, LLVMCore.LLVMInt64TypeInContext(gen.context), "zext.i64");
        }
        return LLVMCore.LLVMBuildTrunc(gen.builder, v, LLVMCore.LLVMInt64TypeInContext(gen.context), "trunc.i64");
    }
    if (LLVMCore.LLVMGetTypeKind(ty) == LLVMTypes.LLVMTypeKind.LLVMDoubleTypeKind) {
        return LLVMCore.LLVMBuildFPToSI(gen.builder, v, LLVMCore.LLVMInt64TypeInContext(gen.context), "fp2si.i64");
    }
    return v;
}

pub fn ensureF64(gen: *LLVMGenerator, v: LLVMTypes.LLVMValueRef) LLVMTypes.LLVMValueRef {
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

pub fn ensureI8(gen: *LLVMGenerator, v: LLVMTypes.LLVMValueRef) LLVMTypes.LLVMValueRef {
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

pub fn ensureI1(gen: *LLVMGenerator, v: LLVMTypes.LLVMValueRef) LLVMTypes.LLVMValueRef {
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
