const llvm = @import("llvm");
const LLVMCore = llvm.core;
const LLVMTypes = llvm.types;
const LLVMGenerator = @import("../llvm.zig").LLVMGenerator;
const externs = @import("externs.zig");
const strings = @import("strings.zig");
const cast = @import("cast.zig");

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
            const fmt = strings.createStringPtr(gen, "%s", next_string_id) catch return;
            var args = [_]LLVMTypes.LLVMValueRef{ fmt, v };
            _ = LLVMCore.LLVMBuildCall2(gen.builder, printf_ty, printf_fn, &args, 2, "printf");
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
            } else if (width == 8) {
                const fmt = strings.createStringPtr(gen, "%u", next_string_id) catch return;
                const i32_ty = LLVMCore.LLVMInt32TypeInContext(gen.context);
                const v32 = LLVMCore.LLVMBuildZExt(gen.builder, v, i32_ty, "zext.i32");
                var args = [_]LLVMTypes.LLVMValueRef{ fmt, v32 };
                _ = LLVMCore.LLVMBuildCall2(gen.builder, printf_ty, printf_fn, &args, 2, "printf");
            } else if (width == 2) {
                const fmt = strings.createStringPtr(gen, "%ld", next_string_id) catch return;
                const v64 = LLVMCore.LLVMBuildZExt(gen.builder, v, LLVMCore.LLVMInt64TypeInContext(gen.context), "zext.i64");
                var args = [_]LLVMTypes.LLVMValueRef{ fmt, v64 };
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
