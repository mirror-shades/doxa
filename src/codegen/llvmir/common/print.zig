const llvm = @import("llvm");
const LLVMCore = llvm.core;
const LLVMTypes = llvm.types;
const LLVMGenerator = @import("../llvm.zig").LLVMGenerator;
const externs = @import("externs.zig");
const strings = @import("strings.zig");
const cast = @import("cast.zig");

fn currentFunction(gen: *LLVMGenerator) ?LLVMTypes.LLVMValueRef {
    const bb = LLVMCore.LLVMGetInsertBlock(gen.builder);
    if (bb == null) return null;
    return LLVMCore.LLVMGetBasicBlockParent(bb);
}

fn printTetra(gen: *LLVMGenerator, v: LLVMTypes.LLVMValueRef, next_string_id: *usize) void {
    const i64_ty = LLVMCore.LLVMInt64TypeInContext(gen.context);
    const void_ty = LLVMCore.LLVMVoidTypeInContext(gen.context);
    const val64 = LLVMCore.LLVMBuildZExt(gen.builder, v, i64_ty, "tetra.zext");
    const func = currentFunction(gen) orelse {
        const print_i64 = externs.getOrCreateDoxaPrintI64(gen);
        var params = [_]LLVMTypes.LLVMTypeRef{i64_ty};
        const fn_ty = LLVMCore.LLVMFunctionType(void_ty, &params, 1, @intFromBool(false));
        var args = [_]LLVMTypes.LLVMValueRef{val64};
        _ = LLVMCore.LLVMBuildCall2(gen.builder, fn_ty, print_i64, &args, 1, "");
        printNewline(gen, next_string_id);
        return;
    };
    const default_block = LLVMCore.LLVMAppendBasicBlockInContext(gen.context, func, "tetra.default");
    const merge_block = LLVMCore.LLVMAppendBasicBlockInContext(gen.context, func, "tetra.merge");
    const switch_inst = LLVMCore.LLVMBuildSwitch(gen.builder, val64, default_block, 4);

    const case0 = LLVMCore.LLVMAppendBasicBlockInContext(gen.context, func, "tetra.case0");
    const case1 = LLVMCore.LLVMAppendBasicBlockInContext(gen.context, func, "tetra.case1");
    const case2 = LLVMCore.LLVMAppendBasicBlockInContext(gen.context, func, "tetra.case2");
    const case3 = LLVMCore.LLVMAppendBasicBlockInContext(gen.context, func, "tetra.case3");

    LLVMCore.LLVMAddCase(switch_inst, LLVMCore.LLVMConstInt(i64_ty, 0, 0), case0);
    LLVMCore.LLVMAddCase(switch_inst, LLVMCore.LLVMConstInt(i64_ty, 1, 0), case1);
    LLVMCore.LLVMAddCase(switch_inst, LLVMCore.LLVMConstInt(i64_ty, 2, 0), case2);
    LLVMCore.LLVMAddCase(switch_inst, LLVMCore.LLVMConstInt(i64_ty, 3, 0), case3);

    // case0: "false"
    LLVMCore.LLVMPositionBuilderAtEnd(gen.builder, case0);
    printLiteralString(gen, "false", next_string_id);
    printNewline(gen, next_string_id);
    _ = LLVMCore.LLVMBuildBr(gen.builder, merge_block);
    // case1: "true"
    LLVMCore.LLVMPositionBuilderAtEnd(gen.builder, case1);
    printLiteralString(gen, "true", next_string_id);
    printNewline(gen, next_string_id);
    _ = LLVMCore.LLVMBuildBr(gen.builder, merge_block);
    // case2: "both"
    LLVMCore.LLVMPositionBuilderAtEnd(gen.builder, case2);
    printLiteralString(gen, "both", next_string_id);
    printNewline(gen, next_string_id);
    _ = LLVMCore.LLVMBuildBr(gen.builder, merge_block);
    // case3: "neither"
    LLVMCore.LLVMPositionBuilderAtEnd(gen.builder, case3);
    printLiteralString(gen, "neither", next_string_id);
    printNewline(gen, next_string_id);
    _ = LLVMCore.LLVMBuildBr(gen.builder, merge_block);

    // default: print numeric fallback
    LLVMCore.LLVMPositionBuilderAtEnd(gen.builder, default_block);
    const print_i64 = externs.getOrCreateDoxaPrintI64(gen);
    var int_params = [_]LLVMTypes.LLVMTypeRef{i64_ty};
    const int_ty = LLVMCore.LLVMFunctionType(void_ty, &int_params, 1, @intFromBool(false));
    var int_args = [_]LLVMTypes.LLVMValueRef{val64};
    _ = LLVMCore.LLVMBuildCall2(gen.builder, int_ty, print_i64, &int_args, 1, "");
    printNewline(gen, next_string_id);
    _ = LLVMCore.LLVMBuildBr(gen.builder, merge_block);

    LLVMCore.LLVMPositionBuilderAtEnd(gen.builder, merge_block);
}

pub fn printStringPtr(gen: *LLVMGenerator, str_ptr: LLVMTypes.LLVMValueRef, next_string_id: *usize) void {
    _ = next_string_id;
    const write_fn = externs.getOrCreateDoxaWriteCStr(gen);
    const void_ty = LLVMCore.LLVMVoidTypeInContext(gen.context);
    const i8_ptr_ty = LLVMCore.LLVMPointerType(LLVMCore.LLVMInt8TypeInContext(gen.context), 0);
    var params = [_]LLVMTypes.LLVMTypeRef{i8_ptr_ty};
    const fn_ty = LLVMCore.LLVMFunctionType(void_ty, &params, 1, @intFromBool(false));
    var args = [_]LLVMTypes.LLVMValueRef{str_ptr};
    _ = LLVMCore.LLVMBuildCall2(gen.builder, fn_ty, write_fn, &args, 1, "");
}

pub fn printLiteralString(gen: *LLVMGenerator, s: []const u8, next_string_id: *usize) void {
    const str = strings.createStringPtr(gen, s, next_string_id) catch return;
    const write_fn = externs.getOrCreateDoxaWriteCStr(gen);
    const void_ty = LLVMCore.LLVMVoidTypeInContext(gen.context);
    const i8_ptr_ty = LLVMCore.LLVMPointerType(LLVMCore.LLVMInt8TypeInContext(gen.context), 0);
    var params = [_]LLVMTypes.LLVMTypeRef{i8_ptr_ty};
    const fn_ty = LLVMCore.LLVMFunctionType(void_ty, &params, 1, @intFromBool(false));
    var args = [_]LLVMTypes.LLVMValueRef{str};
    _ = LLVMCore.LLVMBuildCall2(gen.builder, fn_ty, write_fn, &args, 1, "");
}

pub fn printArray(gen: *LLVMGenerator, arr: LLVMTypes.LLVMValueRef, _: *usize) void {
    const arrays = @import("arrays.zig");
    // Ensure the struct is declared in the module for typedef identity
    _ = arrays.ensureArrayHeaderBody(gen);
    const hdr_ptr_ty = arrays.getArrayHeaderPtrType(gen);

    // Unused now that we call into runtime directly

    // Delegate to runtime helper for compact IR
    const arr_hdr = if (LLVMCore.LLVMTypeOf(arr) == hdr_ptr_ty) arr else LLVMCore.LLVMBuildBitCast(gen.builder, arr, hdr_ptr_ty, "arr.hdr.cast");
    const rt = externs.getOrCreateDoxaPrintArray(gen);
    var args = [_]LLVMTypes.LLVMValueRef{arr_hdr};
    const void_ty = LLVMCore.LLVMVoidTypeInContext(gen.context);
    var params = [_]LLVMTypes.LLVMTypeRef{hdr_ptr_ty};
    const rt_ty = LLVMCore.LLVMFunctionType(void_ty, &params, 1, @intFromBool(false));
    _ = LLVMCore.LLVMBuildCall2(gen.builder, rt_ty, rt, &args, 1, "");
    return;
}

pub fn printNewline(gen: *LLVMGenerator, next_string_id: *usize) void {
    const nl = strings.createStringPtr(gen, "\n", next_string_id) catch return;
    const write_fn = externs.getOrCreateDoxaWriteCStr(gen);
    const void_ty = LLVMCore.LLVMVoidTypeInContext(gen.context);
    const i8_ptr_ty = LLVMCore.LLVMPointerType(LLVMCore.LLVMInt8TypeInContext(gen.context), 0);
    var params = [_]LLVMTypes.LLVMTypeRef{i8_ptr_ty};
    const fn_ty = LLVMCore.LLVMFunctionType(void_ty, &params, 1, @intFromBool(false));
    var args = [_]LLVMTypes.LLVMValueRef{nl};
    _ = LLVMCore.LLVMBuildCall2(gen.builder, fn_ty, write_fn, &args, 1, "");
}

pub fn printValue(gen: *LLVMGenerator, v: LLVMTypes.LLVMValueRef, next_string_id: *usize) void {
    const ty = LLVMCore.LLVMTypeOf(v);
    const kind = LLVMCore.LLVMGetTypeKind(ty);

    switch (kind) {
        LLVMTypes.LLVMTypeKind.LLVMPointerTypeKind => {
            const hdr_ty = @import("arrays.zig").ensureArrayHeaderBody(gen);
            const is_array_header_ptr = LLVMCore.LLVMTypeOf(v) == LLVMCore.LLVMPointerType(hdr_ty, 0);
            if (is_array_header_ptr) {
                printArray(gen, v, next_string_id);
            } else {
                const write_fn = externs.getOrCreateDoxaWriteCStr(gen);
                const void_ty = LLVMCore.LLVMVoidTypeInContext(gen.context);
                const i8_ptr_ty = LLVMCore.LLVMPointerType(LLVMCore.LLVMInt8TypeInContext(gen.context), 0);
                var params = [_]LLVMTypes.LLVMTypeRef{i8_ptr_ty};
                const fn_ty = LLVMCore.LLVMFunctionType(void_ty, &params, 1, @intFromBool(false));
                var args = [_]LLVMTypes.LLVMValueRef{v};
                _ = LLVMCore.LLVMBuildCall2(gen.builder, fn_ty, write_fn, &args, 1, "");
            }
        },
        LLVMTypes.LLVMTypeKind.LLVMDoubleTypeKind, LLVMTypes.LLVMTypeKind.LLVMFloatTypeKind => {
            const vf = cast.ensureF64(gen, v);
            const print_fn = externs.getOrCreateDoxaPrintF64(gen);
            const void_ty = LLVMCore.LLVMVoidTypeInContext(gen.context);
            const f64_ty = LLVMCore.LLVMDoubleTypeInContext(gen.context);
            var params = [_]LLVMTypes.LLVMTypeRef{f64_ty};
            const fn_ty = LLVMCore.LLVMFunctionType(void_ty, &params, 1, @intFromBool(false));
            var args = [_]LLVMTypes.LLVMValueRef{vf};
            _ = LLVMCore.LLVMBuildCall2(gen.builder, fn_ty, print_fn, &args, 1, "");
        },
        LLVMTypes.LLVMTypeKind.LLVMIntegerTypeKind => {
            const width = LLVMCore.LLVMGetIntTypeWidth(ty);
            if (width == 1) {
                const i64_ty = LLVMCore.LLVMInt64TypeInContext(gen.context);
                const v64 = LLVMCore.LLVMBuildZExt(gen.builder, v, i64_ty, "zext.i64");
                const print_fn = externs.getOrCreateDoxaPrintI64(gen);
                var params = [_]LLVMTypes.LLVMTypeRef{i64_ty};
                const fn_ty = LLVMCore.LLVMFunctionType(LLVMCore.LLVMVoidTypeInContext(gen.context), &params, 1, @intFromBool(false));
                var args = [_]LLVMTypes.LLVMValueRef{v64};
                _ = LLVMCore.LLVMBuildCall2(gen.builder, fn_ty, print_fn, &args, 1, "");
            } else if (width == 2) {
                // Tetra pretty print
                printTetra(gen, v, next_string_id);
            } else if (width == 8) {
                const i64_ty = LLVMCore.LLVMInt64TypeInContext(gen.context);
                const v64 = LLVMCore.LLVMBuildZExt(gen.builder, v, i64_ty, "zext.u64");
                const print_fn = externs.getOrCreateDoxaPrintU64(gen);
                var params = [_]LLVMTypes.LLVMTypeRef{i64_ty};
                const fn_ty = LLVMCore.LLVMFunctionType(LLVMCore.LLVMVoidTypeInContext(gen.context), &params, 1, @intFromBool(false));
                var args = [_]LLVMTypes.LLVMValueRef{v64};
                _ = LLVMCore.LLVMBuildCall2(gen.builder, fn_ty, print_fn, &args, 1, "");
            } else {
                const v64 = LLVMCore.LLVMBuildSExt(gen.builder, v, LLVMCore.LLVMInt64TypeInContext(gen.context), "sext.i64");
                const print_fn = externs.getOrCreateDoxaPrintI64(gen);
                const i64_ty = LLVMCore.LLVMInt64TypeInContext(gen.context);
                var params = [_]LLVMTypes.LLVMTypeRef{i64_ty};
                const fn_ty = LLVMCore.LLVMFunctionType(LLVMCore.LLVMVoidTypeInContext(gen.context), &params, 1, @intFromBool(false));
                var args = [_]LLVMTypes.LLVMValueRef{v64};
                _ = LLVMCore.LLVMBuildCall2(gen.builder, fn_ty, print_fn, &args, 1, "");
            }
        },
        else => {},
    }
}
