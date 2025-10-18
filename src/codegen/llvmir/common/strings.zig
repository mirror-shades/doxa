const std = @import("std");
const llvm = @import("llvm");
const LLVMCore = llvm.core;
const LLVMTypes = llvm.types;
const LLVMGenerator = @import("../llvm.zig").LLVMGenerator;

pub fn createStringPtr(gen: *LLVMGenerator, s: []const u8, next_string_id: *usize) !LLVMTypes.LLVMValueRef {
    var buf = try gen.allocator.alloc(u8, s.len + 1);
    defer gen.allocator.free(buf);
    @memcpy(buf[0..s.len], s);
    buf[s.len] = 0;

    const str_val = LLVMCore.LLVMConstStringInContext(gen.context, @ptrCast(buf.ptr), @intCast(buf.len), @intFromBool(false));

    var name_buf: [32]u8 = undefined;
    const id = next_string_id.*;
    next_string_id.* += 1;
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
