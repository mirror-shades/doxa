const llvm = @import("llvm");
const LLVMCore = llvm.core;
const LLVMTypes = llvm.types;
const LLVMGenerator = @import("../llvm.zig").LLVMGenerator;
const arrays = @import("arrays.zig");

var peek_info_ty: LLVMTypes.LLVMTypeRef = null;
var peek_info_ptr_ty: LLVMTypes.LLVMTypeRef = null;

pub fn getPeekInfoType(gen: *LLVMGenerator) LLVMTypes.LLVMTypeRef {
    if (peek_info_ty != null) return peek_info_ty;
    const i8_ptr = LLVMCore.LLVMPointerType(LLVMCore.LLVMInt8TypeInContext(gen.context), 0);
    const i8_ptr_ptr = LLVMCore.LLVMPointerType(i8_ptr, 0);
    const i32_ty = LLVMCore.LLVMInt32TypeInContext(gen.context);
    var fields = [_]LLVMTypes.LLVMTypeRef{
        i8_ptr,
        i8_ptr,
        i8_ptr,
        i8_ptr_ptr,
        i32_ty,
        i32_ty,
        i32_ty,
        i32_ty,
        i32_ty,
    };
    peek_info_ty = LLVMCore.LLVMStructTypeInContext(gen.context, &fields, fields.len, @intFromBool(false));
    return peek_info_ty;
}

pub fn getPeekInfoPtrType(gen: *LLVMGenerator) LLVMTypes.LLVMTypeRef {
    if (peek_info_ptr_ty != null) return peek_info_ptr_ty;
    peek_info_ptr_ty = LLVMCore.LLVMPointerType(getPeekInfoType(gen), 0);
    return peek_info_ptr_ty;
}

pub fn getOrCreatePrintf(gen: *LLVMGenerator) LLVMTypes.LLVMValueRef {
    const existing = LLVMCore.LLVMGetNamedFunction(gen.module, "printf");
    if (existing != null) return existing;
    const i32_ty = LLVMCore.LLVMInt32TypeInContext(gen.context);
    const i8_ptr_ty = LLVMCore.LLVMPointerType(LLVMCore.LLVMInt8TypeInContext(gen.context), 0);
    var params = [_]LLVMTypes.LLVMTypeRef{i8_ptr_ty};
    const printf_ty = LLVMCore.LLVMFunctionType(i32_ty, &params, 1, @intFromBool(true));
    return LLVMCore.LLVMAddFunction(gen.module, "printf", printf_ty);
}

pub fn getOrCreateDoxaPrintArray(gen: *LLVMGenerator) LLVMTypes.LLVMValueRef {
    const existing = LLVMCore.LLVMGetNamedFunction(gen.module, "doxa_print_array_hdr");
    if (existing != null) return existing;
    const hdr_ptr = arrays.getArrayHeaderPtrType(gen);
    var params = [_]LLVMTypes.LLVMTypeRef{hdr_ptr};
    const fn_ty = LLVMCore.LLVMFunctionType(LLVMCore.LLVMVoidTypeInContext(gen.context), &params, 1, @intFromBool(false));
    return LLVMCore.LLVMAddFunction(gen.module, "doxa_print_array_hdr", fn_ty);
}

pub fn getOrCreateDoxaArrayNew(gen: *LLVMGenerator) LLVMTypes.LLVMValueRef {
    const existing = LLVMCore.LLVMGetNamedFunction(gen.module, "doxa_array_new");
    if (existing != null) return existing;
    const i64_ty = LLVMCore.LLVMInt64TypeInContext(gen.context);
    const hdr_ptr = arrays.getArrayHeaderPtrType(gen);
    var params = [_]LLVMTypes.LLVMTypeRef{ i64_ty, i64_ty, i64_ty };
    const fn_ty = LLVMCore.LLVMFunctionType(hdr_ptr, &params, 3, @intFromBool(false));
    return LLVMCore.LLVMAddFunction(gen.module, "doxa_array_new", fn_ty);
}

pub fn getOrCreateDoxaArrayLen(gen: *LLVMGenerator) LLVMTypes.LLVMValueRef {
    const existing = LLVMCore.LLVMGetNamedFunction(gen.module, "doxa_array_len");
    if (existing != null) return existing;
    const hdr_ptr = arrays.getArrayHeaderPtrType(gen);
    const i64_ty = LLVMCore.LLVMInt64TypeInContext(gen.context);
    var params = [_]LLVMTypes.LLVMTypeRef{hdr_ptr};
    const fn_ty = LLVMCore.LLVMFunctionType(i64_ty, &params, 1, @intFromBool(false));
    return LLVMCore.LLVMAddFunction(gen.module, "doxa_array_len", fn_ty);
}

pub fn getOrCreateDoxaArrayGetI64(gen: *LLVMGenerator) LLVMTypes.LLVMValueRef {
    const existing = LLVMCore.LLVMGetNamedFunction(gen.module, "doxa_array_get_i64");
    if (existing != null) return existing;
    const hdr_ptr = arrays.getArrayHeaderPtrType(gen);
    const i64_ty = LLVMCore.LLVMInt64TypeInContext(gen.context);
    var params = [_]LLVMTypes.LLVMTypeRef{ hdr_ptr, i64_ty };
    const fn_ty = LLVMCore.LLVMFunctionType(i64_ty, &params, 2, @intFromBool(false));
    return LLVMCore.LLVMAddFunction(gen.module, "doxa_array_get_i64", fn_ty);
}

pub fn getOrCreateDoxaArraySetI64(gen: *LLVMGenerator) LLVMTypes.LLVMValueRef {
    const existing = LLVMCore.LLVMGetNamedFunction(gen.module, "doxa_array_set_i64");
    if (existing != null) return existing;
    const hdr_ptr = arrays.getArrayHeaderPtrType(gen);
    const i64_ty = LLVMCore.LLVMInt64TypeInContext(gen.context);
    var params = [_]LLVMTypes.LLVMTypeRef{ hdr_ptr, i64_ty, i64_ty };
    const fn_ty = LLVMCore.LLVMFunctionType(LLVMCore.LLVMVoidTypeInContext(gen.context), &params, 3, @intFromBool(false));
    return LLVMCore.LLVMAddFunction(gen.module, "doxa_array_set_i64", fn_ty);
}

pub fn getOrCreateDoxaWriteCStr(gen: *LLVMGenerator) LLVMTypes.LLVMValueRef {
    const existing = LLVMCore.LLVMGetNamedFunction(gen.module, "doxa_write_cstr");
    if (existing != null) return existing;
    const void_ty = LLVMCore.LLVMVoidTypeInContext(gen.context);
    const i8_ptr_ty = LLVMCore.LLVMPointerType(LLVMCore.LLVMInt8TypeInContext(gen.context), 0);
    var params = [_]LLVMTypes.LLVMTypeRef{i8_ptr_ty};
    const fn_ty = LLVMCore.LLVMFunctionType(void_ty, &params, 1, @intFromBool(false));
    return LLVMCore.LLVMAddFunction(gen.module, "doxa_write_cstr", fn_ty);
}

pub fn getOrCreateDoxaPrintI64(gen: *LLVMGenerator) LLVMTypes.LLVMValueRef {
    const existing = LLVMCore.LLVMGetNamedFunction(gen.module, "doxa_print_i64");
    if (existing != null) return existing;
    const void_ty = LLVMCore.LLVMVoidTypeInContext(gen.context);
    const i64_ty = LLVMCore.LLVMInt64TypeInContext(gen.context);
    var params = [_]LLVMTypes.LLVMTypeRef{i64_ty};
    const fn_ty = LLVMCore.LLVMFunctionType(void_ty, &params, 1, @intFromBool(false));
    return LLVMCore.LLVMAddFunction(gen.module, "doxa_print_i64", fn_ty);
}

pub fn getOrCreateDoxaPrintU64(gen: *LLVMGenerator) LLVMTypes.LLVMValueRef {
    const existing = LLVMCore.LLVMGetNamedFunction(gen.module, "doxa_print_u64");
    if (existing != null) return existing;
    const void_ty = LLVMCore.LLVMVoidTypeInContext(gen.context);
    const i64_ty = LLVMCore.LLVMInt64TypeInContext(gen.context);
    var params = [_]LLVMTypes.LLVMTypeRef{i64_ty};
    const fn_ty = LLVMCore.LLVMFunctionType(void_ty, &params, 1, @intFromBool(false));
    return LLVMCore.LLVMAddFunction(gen.module, "doxa_print_u64", fn_ty);
}

pub fn getOrCreateDoxaPrintF64(gen: *LLVMGenerator) LLVMTypes.LLVMValueRef {
    const existing = LLVMCore.LLVMGetNamedFunction(gen.module, "doxa_print_f64");
    if (existing != null) return existing;
    const void_ty = LLVMCore.LLVMVoidTypeInContext(gen.context);
    const f64_ty = LLVMCore.LLVMDoubleTypeInContext(gen.context);
    var params = [_]LLVMTypes.LLVMTypeRef{f64_ty};
    const fn_ty = LLVMCore.LLVMFunctionType(void_ty, &params, 1, @intFromBool(false));
    return LLVMCore.LLVMAddFunction(gen.module, "doxa_print_f64", fn_ty);
}

pub fn getOrCreateDoxaDebugPeek(gen: *LLVMGenerator) LLVMTypes.LLVMValueRef {
    const existing = LLVMCore.LLVMGetNamedFunction(gen.module, "doxa_debug_peek");
    if (existing != null) return existing;
    const void_ty = LLVMCore.LLVMVoidTypeInContext(gen.context);
    var params = [_]LLVMTypes.LLVMTypeRef{getPeekInfoPtrType(gen)};
    const fn_ty = LLVMCore.LLVMFunctionType(void_ty, &params, 1, @intFromBool(false));
    return LLVMCore.LLVMAddFunction(gen.module, "doxa_debug_peek", fn_ty);
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

pub fn getOrCreateMalloc(gen: *LLVMGenerator) LLVMTypes.LLVMValueRef {
    const existing = LLVMCore.LLVMGetNamedFunction(gen.module, "malloc");
    if (existing != null) return existing;
    const i8_ptr = LLVMCore.LLVMPointerType(LLVMCore.LLVMInt8TypeInContext(gen.context), 0);
    const usize_ty = LLVMCore.LLVMInt64TypeInContext(gen.context); // assume 64-bit for now
    var params = [_]LLVMTypes.LLVMTypeRef{usize_ty};
    const fn_ty = LLVMCore.LLVMFunctionType(i8_ptr, &params, 1, @intFromBool(false));
    return LLVMCore.LLVMAddFunction(gen.module, "malloc", fn_ty);
}

pub fn getOrCreateRealloc(gen: *LLVMGenerator) LLVMTypes.LLVMValueRef {
    const existing = LLVMCore.LLVMGetNamedFunction(gen.module, "realloc");
    if (existing != null) return existing;
    const i8_ptr = LLVMCore.LLVMPointerType(LLVMCore.LLVMInt8TypeInContext(gen.context), 0);
    const usize_ty = LLVMCore.LLVMInt64TypeInContext(gen.context);
    var params = [_]LLVMTypes.LLVMTypeRef{ i8_ptr, usize_ty };
    const fn_ty = LLVMCore.LLVMFunctionType(i8_ptr, &params, 2, @intFromBool(false));
    return LLVMCore.LLVMAddFunction(gen.module, "realloc", fn_ty);
}

pub fn getOrCreateFree(gen: *LLVMGenerator) LLVMTypes.LLVMValueRef {
    const existing = LLVMCore.LLVMGetNamedFunction(gen.module, "free");
    if (existing != null) return existing;
    const i8_ptr = LLVMCore.LLVMPointerType(LLVMCore.LLVMInt8TypeInContext(gen.context), 0);
    var params = [_]LLVMTypes.LLVMTypeRef{i8_ptr};
    const fn_ty = LLVMCore.LLVMFunctionType(LLVMCore.LLVMVoidTypeInContext(gen.context), &params, 1, @intFromBool(false));
    return LLVMCore.LLVMAddFunction(gen.module, "free", fn_ty);
}

pub fn getOrCreateMemcpy(gen: *LLVMGenerator) LLVMTypes.LLVMValueRef {
    const existing = LLVMCore.LLVMGetNamedFunction(gen.module, "memcpy");
    if (existing != null) return existing;
    const void_ptr = LLVMCore.LLVMPointerType(LLVMCore.LLVMInt8TypeInContext(gen.context), 0);
    const usize_ty = LLVMCore.LLVMInt64TypeInContext(gen.context);
    var params = [_]LLVMTypes.LLVMTypeRef{ void_ptr, void_ptr, usize_ty };
    const fn_ty = LLVMCore.LLVMFunctionType(void_ptr, &params, 3, @intFromBool(false));
    return LLVMCore.LLVMAddFunction(gen.module, "memcpy", fn_ty);
}

pub fn getOrCreateMemmove(gen: *LLVMGenerator) LLVMTypes.LLVMValueRef {
    const existing = LLVMCore.LLVMGetNamedFunction(gen.module, "memmove");
    if (existing != null) return existing;
    const void_ptr = LLVMCore.LLVMPointerType(LLVMCore.LLVMInt8TypeInContext(gen.context), 0);
    const usize_ty = LLVMCore.LLVMInt64TypeInContext(gen.context);
    var params = [_]LLVMTypes.LLVMTypeRef{ void_ptr, void_ptr, usize_ty };
    const fn_ty = LLVMCore.LLVMFunctionType(void_ptr, &params, 3, @intFromBool(false));
    return LLVMCore.LLVMAddFunction(gen.module, "memmove", fn_ty);
}

pub fn getOrCreateMemset(gen: *LLVMGenerator) LLVMTypes.LLVMValueRef {
    const existing = LLVMCore.LLVMGetNamedFunction(gen.module, "memset");
    if (existing != null) return existing;
    const void_ptr = LLVMCore.LLVMPointerType(LLVMCore.LLVMInt8TypeInContext(gen.context), 0);
    const i32_ty = LLVMCore.LLVMInt32TypeInContext(gen.context);
    const usize_ty = LLVMCore.LLVMInt64TypeInContext(gen.context);
    var params = [_]LLVMTypes.LLVMTypeRef{ void_ptr, i32_ty, usize_ty };
    const fn_ty = LLVMCore.LLVMFunctionType(void_ptr, &params, 3, @intFromBool(false));
    return LLVMCore.LLVMAddFunction(gen.module, "memset", fn_ty);
}
