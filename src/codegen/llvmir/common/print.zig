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
    const val64 = LLVMCore.LLVMBuildZExt(gen.builder, v, i64_ty, "tetra.zext");
    const func = currentFunction(gen) orelse {
        // Fallback: print numeric if no active function context
        const fmt = strings.createStringPtr(gen, "%ld", next_string_id) catch return;
        var args = [_]LLVMTypes.LLVMValueRef{ fmt, val64 };
        const printf_fn = externs.getOrCreatePrintf(gen);
        const printf_ty = getPrintfType(gen);
        _ = LLVMCore.LLVMBuildCall2(gen.builder, printf_ty, printf_fn, &args, 2, "printf");
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
    const fmt = strings.createStringPtr(gen, "%ld", next_string_id) catch return;
    var args = [_]LLVMTypes.LLVMValueRef{ fmt, val64 };
    const printf_fn = externs.getOrCreatePrintf(gen);
    const printf_ty = getPrintfType(gen);
    _ = LLVMCore.LLVMBuildCall2(gen.builder, printf_ty, printf_fn, &args, 2, "printf");
    printNewline(gen, next_string_id);
    _ = LLVMCore.LLVMBuildBr(gen.builder, merge_block);

    LLVMCore.LLVMPositionBuilderAtEnd(gen.builder, merge_block);
}

fn getPrintfType(gen: *LLVMGenerator) LLVMTypes.LLVMTypeRef {
    const i32_ty = LLVMCore.LLVMInt32TypeInContext(gen.context);
    const i8_ptr_ty = LLVMCore.LLVMPointerType(LLVMCore.LLVMInt8TypeInContext(gen.context), 0);
    var params = [_]LLVMTypes.LLVMTypeRef{i8_ptr_ty};
    return LLVMCore.LLVMFunctionType(i32_ty, &params, 1, @intFromBool(true));
}

pub fn printStringPtr(gen: *LLVMGenerator, str_ptr: LLVMTypes.LLVMValueRef, next_string_id: *usize) void {
    const printf_fn = externs.getOrCreatePrintf(gen);
    const printf_ty = getPrintfType(gen);
    const fmt = strings.createStringPtr(gen, "%s", next_string_id) catch return;
    var args = [_]LLVMTypes.LLVMValueRef{ fmt, str_ptr };
    _ = LLVMCore.LLVMBuildCall2(gen.builder, printf_ty, printf_fn, &args, 2, "printf");
}

pub fn printLiteralString(gen: *LLVMGenerator, s: []const u8, next_string_id: *usize) void {
    const printf_fn = externs.getOrCreatePrintf(gen);
    const printf_ty = getPrintfType(gen);
    const fmt = strings.createStringPtr(gen, "%s", next_string_id) catch return;
    const str = strings.createStringPtr(gen, s, next_string_id) catch return;
    var args = [_]LLVMTypes.LLVMValueRef{ fmt, str };
    _ = LLVMCore.LLVMBuildCall2(gen.builder, printf_ty, printf_fn, &args, 2, "printf");
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
    const printf_fn = externs.getOrCreatePrintf(gen);
    const printf_ty = getPrintfType(gen);
    const nl = strings.createStringPtr(gen, "\n", next_string_id) catch return;
    var args = [_]LLVMTypes.LLVMValueRef{nl};
    _ = LLVMCore.LLVMBuildCall2(gen.builder, printf_ty, printf_fn, &args, 1, "printf");
}

pub fn printValue(gen: *LLVMGenerator, v: LLVMTypes.LLVMValueRef, next_string_id: *usize) void {
    const ty = LLVMCore.LLVMTypeOf(v);
    const kind = LLVMCore.LLVMGetTypeKind(ty);
    const printf_fn = externs.getOrCreatePrintf(gen);
    const printf_ty = getPrintfType(gen);

    switch (kind) {
        LLVMTypes.LLVMTypeKind.LLVMPointerTypeKind => {
            const hdr_ty = @import("arrays.zig").ensureArrayHeaderBody(gen);
            const is_array_header_ptr = LLVMCore.LLVMTypeOf(v) == LLVMCore.LLVMPointerType(hdr_ty, 0);
            if (is_array_header_ptr) {
                printArray(gen, v, next_string_id);
            } else {
                const fmt = strings.createStringPtr(gen, "%s", next_string_id) catch return;
                var args = [_]LLVMTypes.LLVMValueRef{ fmt, v };
                _ = LLVMCore.LLVMBuildCall2(gen.builder, printf_ty, printf_fn, &args, 2, "printf");
            }
        },
        LLVMTypes.LLVMTypeKind.LLVMDoubleTypeKind, LLVMTypes.LLVMTypeKind.LLVMFloatTypeKind => {
            const fmt = strings.createStringPtr(gen, "%g", next_string_id) catch return;
            const vf = cast.ensureF64(gen, v);
            var args = [_]LLVMTypes.LLVMValueRef{ fmt, vf };
            _ = LLVMCore.LLVMBuildCall2(gen.builder, printf_ty, printf_fn, &args, 2, "printf");
        },
        LLVMTypes.LLVMTypeKind.LLVMIntegerTypeKind => {
            const width = LLVMCore.LLVMGetIntTypeWidth(ty);
            if (width == 1) {
                const fmt = strings.createStringPtr(gen, "%d", next_string_id) catch return;
                const i32_ty = LLVMCore.LLVMInt32TypeInContext(gen.context);
                const v32 = LLVMCore.LLVMBuildZExt(gen.builder, v, i32_ty, "zext.i32");
                var args = [_]LLVMTypes.LLVMValueRef{ fmt, v32 };
                _ = LLVMCore.LLVMBuildCall2(gen.builder, printf_ty, printf_fn, &args, 2, "printf");
            } else if (width == 2) {
                // Tetra pretty print
                printTetra(gen, v, next_string_id);
            } else if (width == 8) {
                const fmt = strings.createStringPtr(gen, "%u", next_string_id) catch return;
                const i32_ty = LLVMCore.LLVMInt32TypeInContext(gen.context);
                const v32 = LLVMCore.LLVMBuildZExt(gen.builder, v, i32_ty, "zext.i32");
                var args = [_]LLVMTypes.LLVMValueRef{ fmt, v32 };
                _ = LLVMCore.LLVMBuildCall2(gen.builder, printf_ty, printf_fn, &args, 2, "printf");
            } else {
                const fmt = strings.createStringPtr(gen, "%ld", next_string_id) catch return;
                const v64 = LLVMCore.LLVMBuildSExt(gen.builder, v, LLVMCore.LLVMInt64TypeInContext(gen.context), "sext.i64");
                var args = [_]LLVMTypes.LLVMValueRef{ fmt, v64 };
                _ = LLVMCore.LLVMBuildCall2(gen.builder, printf_ty, printf_fn, &args, 2, "printf");
            }
        },
        else => {},
    }
}
