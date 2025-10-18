const llvm = @import("llvm");
const LLVMCore = llvm.core;
const LLVMTypes = llvm.types;
const LLVMGenerator = @import("../llvm.zig").LLVMGenerator;

pub fn getOrCreatePrintf(gen: *LLVMGenerator) LLVMTypes.LLVMValueRef {
    const existing = LLVMCore.LLVMGetNamedFunction(gen.module, "printf");
    if (existing != null) return existing;
    const i32_ty = LLVMCore.LLVMInt32TypeInContext(gen.context);
    const i8_ptr_ty = LLVMCore.LLVMPointerType(LLVMCore.LLVMInt8TypeInContext(gen.context), 0);
    var params = [_]LLVMTypes.LLVMTypeRef{i8_ptr_ty};
    const printf_ty = LLVMCore.LLVMFunctionType(i32_ty, &params, 1, @intFromBool(true));
    return LLVMCore.LLVMAddFunction(gen.module, "printf", printf_ty);
}

pub fn getOrCreatePuts(gen: *LLVMGenerator) LLVMTypes.LLVMValueRef {
    const existing = LLVMCore.LLVMGetNamedFunction(gen.module, "puts");
    if (existing != null) return existing;
    const i32_ty = LLVMCore.LLVMInt32TypeInContext(gen.context);
    const i8_ptr_ty = LLVMCore.LLVMPointerType(LLVMCore.LLVMInt8TypeInContext(gen.context), 0);
    var params = [_]LLVMTypes.LLVMTypeRef{i8_ptr_ty};
    const puts_ty = LLVMCore.LLVMFunctionType(i32_ty, &params, 1, @intFromBool(false));
    return LLVMCore.LLVMAddFunction(gen.module, "puts", puts_ty);
}

pub fn getOrCreatePow(gen: *LLVMGenerator) LLVMTypes.LLVMValueRef {
    const existing = LLVMCore.LLVMGetNamedFunction(gen.module, "pow");
    if (existing != null) return existing;
    const f64_ty = LLVMCore.LLVMDoubleTypeInContext(gen.context);
    var params = [_]LLVMTypes.LLVMTypeRef{ f64_ty, f64_ty };
    const pow_ty = LLVMCore.LLVMFunctionType(f64_ty, &params, 2, @intFromBool(false));
    return LLVMCore.LLVMAddFunction(gen.module, "pow", pow_ty);
}
